import * as React              from "react";
import * as Types              from "./Types";
import CustomLayoutViewer      from "./CustomLayoutViewer";
import TestCaseViewer          from "./TestCaseViewer";
import { useEffect, useState } from "react";

require("./style.css"); /* cause webpack to include style.css */

export default function App() : JSX.Element {
  const [info, setInfo] = useState<Types.Info | undefined>(undefined);

  useEffect(() => {
    const ws = new WebSocket('ws://localhost:1234/ws');

    ws.onopen = () => {
      console.log("Requesting info...");
      ws.send(JSON.stringify({
        op: "RequestInfo"
      }));
    }

    ws.onmessage = (event) => {
      const data = JSON.parse(event.data) as Types.LayoutMsg;
      switch(data.op) {
        case "Info": {
          setInfo(data);
        } break;
        case "Error": {
          console.error(data.message);
        }
      }
      ws.close();
    }

    ws.onerror = (event) => {
      console.log(event);
    }
  }, []);

  if(info !== undefined)  {
    if(info.type === "examples" && info.nExamples !== undefined) {
      const testCases: JSX.Element[] = [];
      for(let i = 0; i < info.nExamples; ++i) {
        testCases.push(<TestCaseViewer
                         key={i.toString()}
                         testCaseIndex={i.toString()}/>);
      }
      return <>{testCases}</>;
    } else if(info.type === "customLayout") {
      return <CustomLayoutViewer/>;
    }
  }

  return <p>loading...</p>;
}
