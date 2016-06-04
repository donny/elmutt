# -*- coding: utf-8 -*-

# Based on https://devcenter.heroku.com/articles/python-websockets

import os
import logging
import redis
import gevent
import json
import uuid
from flask import Flask, render_template
from flask_sockets import Sockets
import redisco
from redisco import models
import urlparse

REDIS_URL = os.environ['REDIS_URL']
REDIS_CHAN = 'channel'

app = Flask(__name__)
app.debug = True  # 'DEBUG' in os.environ

sockets = Sockets(app)
redis = redis.from_url(REDIS_URL)


class BackEnd(object):
    """Interface for registering and updating WebSocket clients."""

    def __init__(self):
        self.clients = list()
        self.pubsub = redis.pubsub()
        self.pubsub.subscribe(REDIS_CHAN)
        redis_url = urlparse.urlparse(REDIS_URL)
        redisco.connection_setup(
            host=redis_url.hostname, port=redis_url.port, password=redis_url.password)

    def __iter_data(self):
        for message in self.pubsub.listen():
            data = message.get('data')
            if message['type'] == 'message':
                app.logger.info(u'Sending message: {}'.format(data))
                yield data

    def register(self, client):
        """Register a WebSocket connection for Redis updates."""
        self.clients.append(client)

    def send(self, client, data):
        """Send given data to the registered client.
        Automatically discards invalid connections."""
        try:
            client.send(data)
        except Exception:
            self.clients.remove(client)

    def run(self):
        """Listens for new messages in Redis, and sends them to clients."""
        for data in self.__iter_data():
            for client in self.clients:
                gevent.spawn(self.send, client, data)

    def start(self):
        """Maintains Redis subscription in the background."""
        gevent.spawn(self.run)


backend = BackEnd()
backend.start()


@app.route('/')
def hello():
    return render_template('index.html')


@sockets.route('/submit')
def inbox(ws):
    """Receives incoming chat messages, inserts them into Redis."""
    while not ws.closed:
        # Sleep to prevent *constant* context-switches.
        gevent.sleep(0.1)
        raw_message = ws.receive()

        if raw_message:
            app.logger.info(u'Inserting message: {}'.format(raw_message))
            message = json.loads(raw_message)
            result = process_message(message)
            if result is not None:
                redis.publish(REDIS_CHAN, json.dumps(result))


@sockets.route('/receive')
def outbox(ws):
    """Sends outgoing messages, via `BackEnd`."""
    backend.register(ws)

    while not ws.closed:
        # Context switch while `BackEnd.start` is running in the background.
        gevent.sleep(0.1)


class Card(models.Model):
    identifier = models.Attribute()
    text = models.Attribute()
    counter = models.Counter()


class CardList(models.Model):
    identifier = models.Attribute()
    text = models.Attribute()
    cards = models.ListField(Card)


def process_message(message):
    result = None

    if message['REQ'] == 'REFRESH':
        raw_card_lists = CardList.objects.all()
        all_card_lists = []
        for raw_card_list in raw_card_lists:
            card_list = {'text': raw_card_list.text,
                         'identifier': raw_card_list.identifier, 'cards': []}
            for raw_card in raw_card_list.cards:
                card_list['cards'].append(
                    {'text': raw_card.text, 'identifier': raw_card.identifier, 'counter': raw_card.counter})
            all_card_lists.append(card_list)
        result = {'RESP': 'RESP_REFRESH',
                  'DATA': all_card_lists}

    elif message['REQ'] == 'NEWLIST':
        card_list = CardList(identifier=str(uuid.uuid4()), text="Untitled")
        card_list.save()
        result = {'RESP': 'RESP_NEWLIST',
                  'IDENTIFIER': card_list.identifier,
                  'TEXT': card_list.text}

    elif message['REQ'] == 'RENAMELIST':
        card_list = CardList.objects.filter(
            identifier=message['IDENTIFIER']).first()
        card_list.text = message['TEXT']
        card_list.save()
        result = {'RESP': 'RESP_RENAMELIST',
                  'IDENTIFIER': card_list.identifier,
                  'TEXT': card_list.text}

    elif message['REQ'] == 'NEWCARD':
        card = Card(identifier=str(uuid.uuid4()), text="Untitled")
        card.save()
        card_list = CardList.objects.filter(
            identifier=message['LISTIDENTIFIER']).first()
        card_list.cards.append(card)
        card_list.save()
        result = {'RESP': 'RESP_NEWCARD',
                  'IDENTIFIER': card.identifier,
                  'LISTIDENTIFIER': card_list.identifier,
                  'TEXT': card.text}

    elif message['REQ'] == 'RENAMECARD':
        card = Card.objects.filter(identifier=message['IDENTIFIER']).first()
        card.text = message['TEXT']
        card.save()
        result = {'RESP': 'RESP_RENAMECARD',
                  'IDENTIFIER': card.identifier,
                  'LISTIDENTIFIER': message['LISTIDENTIFIER'],
                  'TEXT': card.text}

    elif message['REQ'] == 'UPVOTECARD':
        card = Card.objects.filter(identifier=message['IDENTIFIER']).first()
        card.incr('counter')
        card.save()
        result = {'RESP': 'RESP_UPVOTECARD',
                  'IDENTIFIER': card.identifier,
                  'LISTIDENTIFIER': message['LISTIDENTIFIER'],
                  'COUNTER': card.counter}

    return result
