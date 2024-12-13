import * as React from "react";
import styles     from "./button.module.css"

interface ButtonProps {
  children: string;
  onClick?: (() => void);
}

export default function Button(props: ButtonProps): JSX.Element {
  return <div className={styles.buttonRoot}>
           <span onClick={() => {
             if(props.onClick) props.onClick();
           }}>
             { props.children }
           </span>
         </div>;
}
