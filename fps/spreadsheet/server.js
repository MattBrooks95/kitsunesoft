const express = require('express')
const http = require('node:http')
const url = require('node:url')
const path = require('node:path')
const dotenv = require('dotenv')

//get configuration from .env file
dotenv.config()

const app = express()

app.use(express.static('public'));

function envOrDie(envName) {
	const env = process.env[envName]
	if (env === undefined || env === '') {
		throw new Error(`failed to get env variable:${envName}`);
	}
	return env;
}

const LOCAL_PORT = envOrDie('LOCAL_SERVER_PORT');
const API_PORT = envOrDie('API_PORT');
const API_HOST = envOrDie('API_HOST');

app.get('/', (req, res) => {
	res.sendFile("index.html", {
		root: path.join(__dirname)
	})
})

app.get('/api', (req, res) => {
	const proxyUrl = `http://${API_HOST}:${API_PORT}/${req.url.replace('/api', '')}`;
	console.log(`rerouting ${req.url} to ${proxyUrl}`);
	const reqToApiServer = http.request(
		proxyUrl,
		(resFromApiServer) => {
			resFromApiServer.pipe(res)
		}
	)
	reqToApiServer.end();
})

app.listen(LOCAL_PORT, () => {
	console.log(`listening on port ${LOCAL_PORT}, proxying to ${API_PORT}`);
})
