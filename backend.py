# -*- coding: utf-8 -*-

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
app.debug = True #'DEBUG' in os.environ

sockets = Sockets(app)
redis = redis.from_url(REDIS_URL)



class BackEnd(object):
    """Interface for registering and updating WebSocket clients."""

    def __init__(self):
        self.clients = list()
        self.pubsub = redis.pubsub()
        self.pubsub.subscribe(REDIS_CHAN)
        redis_url = urlparse.urlparse(REDIS_URL)
        redisco.connection_setup(host=redis_url.hostname, port=redis_url.port, password=redis_url.password)

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

    if message['REQ'] == 'NEWLIST':
        card_list = CardList(identifier=str(uuid.uuid4()), text="Untitled")
        card_list.save()
        result = {'RESP': 'NEWLIST', 'IDENTIFIER': card_list.identifier, 'TEXT': card_list.text}
    elif message['REQ'] == 'RENAMELIST':
        result = {'RESP': 'RENAMELIST', 'IDENTIFIER': message['IDENTIFIER'], 'TEXT': message['TEXT']}
    elif message['REQ'] == 'NEWCARD':
        result = {'RESP': 'NEWCARD', 'IDENTIFIER': str(uuid.uuid4()), 'LISTIDENTIFIER': message['LISTIDENTIFIER'], 'TEXT': 'TEMPCARD Untitled'}
    elif message['REQ'] == 'RENAMECARD':
        result = {'RESP': 'RENAMECARD', 'IDENTIFIER': message['IDENTIFIER'], 'LISTIDENTIFIER': message['LISTIDENTIFIER'], 'TEXT': message['TEXT']}
    elif message['REQ'] == 'UPVOTECARD':
        result = {'RESP': 'UPVOTECARD', 'IDENTIFIER': message['IDENTIFIER'], 'LISTIDENTIFIER': message['LISTIDENTIFIER'], 'COUNTER': 9}

    return result
