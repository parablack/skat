import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import { App } from './App';

const ws = new WebSocket(`ws://${window.location.hostname}:8080`)
ws.onopen = () => console.log("ws opened")
ws.onclose = () => console.log("ws closed")

ReactDOM.render(
  <React.StrictMode>
    <App ws={ws} />
  </React.StrictMode>,
  document.getElementById('root')
);