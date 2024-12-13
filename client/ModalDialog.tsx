import * as React from "react";
import { useState, useRef } from "react";
import styles from "./modal-dialog.module.css";
import Button from "./Button";
import { createPortal } from "react-dom";

export interface ModalDialogProps {
  message: string;
  buttonLabel: string | undefined;
  requireResponse: boolean;
  onCancel: () => void;
  onDone: (response: string | null) => void;
}

export default function ModalDialog(props: ModalDialogProps): JSX.Element {
  const [inputValue, setInputValue] = useState<string>("");
  const opaqueRef = useRef<HTMLDivElement>(null);

  function shouldClose(event: React.MouseEvent) {
    if(event.target === opaqueRef.current) {
      /* Only cancel if the opaque region was clicked */
      props.onCancel();
    }
  }

  function shouldSubmit() {
    if(props.requireResponse) {
      props.onDone(inputValue !== undefined && inputValue.length > 0 ? inputValue : null);
    } else {
      props.onDone(null);
    }
  }

  function onKeyUp(event: React.KeyboardEvent) {
    if(event.key === "Enter" && inputValue.length > 0) {
      shouldSubmit();
    }
  }

  return createPortal(
    <div ref={opaqueRef} onClick={shouldClose} className={styles.opaque}>
      <div className={styles.modal_dialog_root}>
        <span>{props.message}</span>
        {
          props.requireResponse
            ? <div className={styles.input_box}>
                <input
                  value={inputValue}
                  onChange={e => setInputValue(e.target.value)}
                  onKeyUp={onKeyUp}></input>
              </div>
            : <></>
        }
        <div className={styles.button_row}>
          <Button onClick={props.onCancel}>{"Cancel"}</Button>
          <Button onClick={shouldSubmit}>{props.buttonLabel ?? "Done"}</Button>
        </div>
      </div>
    </div>, document.body);
}
