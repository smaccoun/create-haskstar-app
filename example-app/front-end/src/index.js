import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import Cookies from 'js-cookie';

if (window.location.pathname === "/silentCallback") {
  if (document.location.hash === undefined || document.location.hash === null || document.location.hash === "") {
    console.log("COULD NOT FIND HASH")
  } else {
    parent.postMessage(document.location.hash, "*");
  }
} else {
  bootApp();
}

function bootApp() {
  var storedToken = Cookies.get('token');
  console.log("TOKEN! " , storedToken)

  console.log(process.env)
  const apiBaseUrl = process.env.ELM_APP_API_URL
  const environment = process.env.NODE_ENV
  console.log(apiBaseUrl)
  let elmOpts = {
      environment,
      apiBaseUrl,
      jwtToken: null
    };

  if(storedToken){
    elmOpts.jwtToken = storedToken    
  }

  var app = Main.fullscreen(elmOpts);

  app.ports.saveToken.subscribe(function(jwtToken){
    console.log("GOING TO SET THE TOKEN ", jwtToken)
    Cookies.set('token', jwtToken)
    app.ports.receiveToken.send(jwtToken)
  })
}

registerServiceWorker()
