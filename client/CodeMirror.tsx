import * as React from "react";
import { useEffect, useRef } from "react";
import { EditorView, keymap } from "@codemirror/view";
import { Extension } from "@codemirror/state";
import { defaultKeymap } from "@codemirror/commands";
import { basicSetup } from "codemirror";

interface CodeMirrorProps {
  extensions?: Extension[];
  value: string;
  onChange?: (value: string) => void;
}

export default function CodeMirror(props: CodeMirrorProps): JSX.Element {
  const codeMirrorParent = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if(codeMirrorParent.current === null) return;

    const view = new EditorView({
      doc: props.value,
      parent: codeMirrorParent.current,
      extensions: [
        basicSetup,
        keymap.of(defaultKeymap),
        EditorView.updateListener.of(event => {
          if(event.docChanged && props.onChange !== undefined) {
            props.onChange(event.state.doc.sliceString(0));
          }
        })
      ].concat(props.extensions ?? [])
    });

    return () => {
      view.destroy();
    }

  }, []);

  return <div ref={codeMirrorParent}></div>;
}
