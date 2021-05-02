import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import { App } from './App';

let connect_url = window.location.protocol === "https:" ? `wss://${window.location.hostname}/ws` : `ws://${window.location.hostname}:8080`
const ws = new WebSocket(connect_url)
ws.onopen = () => {
  console.log("ws opened")
  let nickname = localStorage.getItem("nickname")
  if (nickname) {
    ws.send(JSON.stringify({
      action: "setname",
      name: nickname,
    }))
  }
}
ws.onclose = () => console.log("ws closed")

ReactDOM.render(
  <React.StrictMode>
    <App ws={ws} />
  </React.StrictMode>,
  document.getElementById('root')
);