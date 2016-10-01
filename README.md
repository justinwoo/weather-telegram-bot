# weather-telegram-bot

A Telegram Bot example in Purescript using Yahoo's Weather API.

## Installation

Install the dependencies and make sure you have `node`, `npm`, `bower`, `pulp` (`npm i -g pulp`), and `psc` (`npm i -g purescript`).

```sh
bower install
npm install
```

Create a `config.json` file with two properties:

```js
{
  "token": "123456:ABCDEF", // your Telegram Bot API token
  "location": "Helsinki, Fi" // or your location
}
```

See [Telegram Bot API docs](https://core.telegram.org/bots/api) for more details.
