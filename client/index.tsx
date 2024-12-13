import * as React from "react";
import * as ReactDOM from "react-dom/client";
import App from "./App";

const root = ReactDOM.createRoot(document.getElementById("react-root")!);
root.render(
  /* Note: React.StrictMode allows React to catch some errors
   *       with react components, but it needs to render the main
   *       component twice to do that. Running webpack with a
   *       production build disables this. */
  <React.StrictMode>
    <App/>
  </React.StrictMode>);
