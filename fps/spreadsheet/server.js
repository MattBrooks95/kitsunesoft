const express = require("express")
const path = require("node:path")

const app = express()

app.use(express.static('public'));

const PORT = 8080;

app.get('/', (req, res) => {
	res.sendFile("index.html", {
		root: path.join(__dirname)
	})
})

app.listen(PORT, () => {
	console.log(`listening on port ${PORT}`);
})
