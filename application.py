from flask import Flask, request
from flask_restful import Resource, Api
import requests

app = Flask(__name__)
api = Api(app)

class HelloWorld(Resource):
    def get(self):
        latitude = request.args.get('latitude')
        longtitude = request.args.get('longtitude')
        uri = "https://api.darksky.net/forecast/bacc57dc0842ea7a771f2dd4e337e87e/" + latitude + "," + longtitude
        response = requests.get(url = uri)
        return response.json()

api.add_resource(HelloWorld, '/forecast')

if __name__ == '__main__':
    app.run(debug=True)
