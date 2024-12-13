import * as React      from "react";
import * as Types      from "./Types";
import Inspector       from "./Inspector";
import StylishTextView from "./StylishTextView";
import measureElement  from "./measureElement";
import styles          from "./test-case-viewer.module.css"
import { useEffect, useState } from "react";

interface TestCaseViewerProps {
  testCaseIndex: String
}

export default function TestCaseViewer({ testCaseIndex }: TestCaseViewerProps) {
  const [layout, setLayout] = useState<Types.LayoutData>();
  const [description, setDescription] = useState<string>("no description");
  const [selectedIds, setSelectedIds] = useState<Set<number>>(new Set());

  useEffect(() => {
    function onKeyDown(event: KeyboardEvent) {
      if(event.key === "Escape") {
        /* Clear inspector points */
        setSelectedIds(new Set());
      }
    }

    window.addEventListener("keydown", onKeyDown);
    return () => {
      window.removeEventListener("keydown", onKeyDown);
    }
  }, []);

  useEffect(() => {
    const ws = new WebSocket('ws://localhost:1234/ws');
    ws.onopen = () => {
      console.log("Requesting layout...");
      ws.send(JSON.stringify({
        op: "RequestExampleLayout",
        exampleIndex: testCaseIndex
      }));
    }

    ws.onmessage = (event) => {
      const data = JSON.parse(event.data) as Types.LayoutMsg;
      switch(data.op) {
        case "Measure": {
          ws.send(JSON.stringify(measureElement(data.spec)));
        } break;
        case "RequestLeading": {
          ws.send(JSON.stringify(measureElement({
            op: "MeasureText",
            text: "A",
            styles: []
          }).height));
        } break;
        case "LayoutDone": {
          ws.send("[]"); /* unit */
          setLayout(data.data);
          setDescription(data.description);
        } break;
        case "Error": {
          console.error(data.message);
        }
      }
    }
  }, []);

  function onNodeClicked(nodeId: number, event: React.MouseEvent) {
    if(event.shiftKey) {
      setSelectedIds(ids => {
        if(ids.has(nodeId)) {
          const newSet = new Set(ids);
          newSet.delete(nodeId);
          return newSet;
        } else {
          const newSet = new Set(ids);
          newSet.add(nodeId);
          return newSet;
        }
      });
    } else {
      setSelectedIds(ids => {
        if(ids.size === 1 && ids.has(nodeId)) {
          return new Set();
        } else {
          return new Set([nodeId]);
        }
      });
    }
  }

  if(layout === undefined) {
    return <div className={styles.testCaseContainer}>
      <p className={styles.testCaseDescription}>{description}</p>
      <p>loading...</p>
    </div>;
  } else {
    return <div className={styles.testCaseContainer}>
             <p className={styles.testCaseDescription}>{description}</p>
             <StylishTextView
               onClick={onNodeClicked}
               inspectorIds={selectedIds}
               layout={layout}/>
             <Inspector
               ids={selectedIds}
               layout={layout}
               onClick={onNodeClicked}/>
           </div>;
  }
}
