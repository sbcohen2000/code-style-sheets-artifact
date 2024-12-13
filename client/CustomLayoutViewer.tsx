import * as React       from "react";
import * as Types       from "./Types";
import CodeMirror       from "./CodeMirror";
import StylishTextView  from "./StylishTextView";
import Button           from "./Button";
import measureElement   from "./measureElement"
import { useState, useEffect, useCallback } from "react";
import { MultiPanelView, View } from "./MultiPanelView";
import * as MB from "./MenuBar";
import * as CM from "./ContextMenu";
import * as MP from "./MultiPanelView";
import styles from "./custom-layout-viewer.module.css";
import { v4 as uuidv4 } from "uuid";
import ModalDialog, { ModalDialogProps } from "./ModalDialog";
import Inspector from "./Inspector";
import { assert } from "./common";

const defaultSource = `
module Main where

main = putStrLn "Hello, Code Style Sheets!"
`;

const defaultStyle = `
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module MyStylesheet where

import StylishText
import qualified Stylesheet as S (selector)
import CHI.Types

selectNumbers :: Selector
selectNumbers = [S.selector|
x@EInt _ ->
x {
  color: orange;
}
|]

stylesheet = [selectNumbers]

eval :: SourceFile -> StylishText
eval e = applyStyles e stylesheet $ showStylish [] e
`;

interface DocumentBase {
  type: string;
  uniqueId: string;
  name: string;
}

type LayoutState =
  { type: "Loading", layout: Promise<void> }
| { type: "Success", layout: Types.LayoutData }
| { type: "None" }
| { type: "Error", error: string };

interface Stylesheet extends DocumentBase {
  type: "Stylesheet";
  source: string;
  sourceIsOpen: boolean;
  layout: LayoutState;
  attachedSourceFileId: string | null;

  /* inspectorPoint{A,B} are the ids of nodes which
   * define the scope of the inspector's tree view */
  inspectorPoints: Set<number>;
}

type TimerId = ReturnType<typeof setTimeout>;

interface SourceFile extends DocumentBase {
  type: "SourceFile";
  source: string;
  lastEditTimer: TimerId | null;
}

type Document = Stylesheet | SourceFile;

type ContextMenuState = {
  menu: CM.MenuTreeSubmenu;
  clientX: number;
  clientY: number;
} | null;

function modifyDocument(uniqueId: string, f: (doc: Document) => Document, documents: Document[]): Document[] {
  return documents.map(doc => {
    if(doc.uniqueId === uniqueId) {
      return f(doc);
    } else {
      return doc;
    }
  });
}

function setStylesheetLayout(uniqueId: string, layout: LayoutState, documents: Document[]): Document[] {
  return modifyDocument(uniqueId, doc => {
    assert(doc.type === "Stylesheet");
    return { ...doc, layout, inspectorPoints: new Set() };
  }, documents);
}

function setStylesheetSource(uniqueId: string, source: string, documents: Document[]): Document[] {
  return modifyDocument(uniqueId, doc => {
    assert(doc.type === "Stylesheet");
    return { ...doc, source };
  }, documents);
}

function setStylesheetSourceIsOpen(uniqueId: string, isOpen: boolean, documents: Document[]): Document[] {
  return modifyDocument(uniqueId, doc => {
    assert(doc.type === "Stylesheet");
    return { ...doc, sourceIsOpen: isOpen };
  }, documents);
}

function toggleStylesheetSourceIsOpen(uniqueId: string, documents: Document[]): Document[] {
  return modifyDocument(uniqueId, doc => {
    assert(doc.type === "Stylesheet");
    return { ...doc, sourceIsOpen: !doc.sourceIsOpen };
  }, documents);
}

function setStylesheetAttachedSourceFile(uniqueId: string, sourceUniqueId: string | null, documents: Document[]): Document[] {
  return modifyDocument(uniqueId, doc => {
    assert(doc.type === "Stylesheet");
    return { ...doc, attachedSourceFileId: sourceUniqueId };
  }, documents);
}

function setStylesheetInspectorPoint(uniqueId: string, node: number, documents: Document[]): Document[] {
  return modifyDocument(uniqueId, doc => {
    assert(doc.type === "Stylesheet");
    if(doc.inspectorPoints.size === 1 && doc.inspectorPoints.has(node)) {
      return { ...doc, inspectorPoints: new Set() };
    } else {
      return { ...doc, inspectorPoints: new Set([node]) };
    }
  }, documents);
}

function addStylesheetInspectorPoint(uniqueId: string, node: number, documents: Document[]): Document[] {
  return modifyDocument(uniqueId, doc => {
    assert(doc.type === "Stylesheet");
    if(doc.inspectorPoints.has(node)) {
      const newSet = new Set(doc.inspectorPoints);
      newSet.delete(node);
      return { ...doc, inspectorPoints: newSet };
    } else {
      const newSet = new Set(doc.inspectorPoints);
      newSet.add(node);
      return { ...doc, inspectorPoints: newSet };
    }
  }, documents);
}

function setSourceFileSource(uniqueId: string, source: string, documents: Document[]): Document[] {
  return modifyDocument(uniqueId, doc => {
    assert(doc.type === "SourceFile");
    return { ...doc, source };
  }, documents);
}

function closeDocument(uniqueId: string, documents: Document[]): Document[] {
  return documents.filter(doc => doc.uniqueId !== uniqueId);
}

function addNewSourceFile(name: string, source: string, id: string | undefined, documents: Document[]): [Document[], SourceFile] {
  const doc: SourceFile = {
    type: "SourceFile",
    name,
    uniqueId: id ?? uuidv4(),
    source,
    lastEditTimer: null
  }

  return [documents.concat(doc), doc];
}

function addNewStylesheet(name: string, source: string, id: string | undefined, documents: Document[]): [Document[], Stylesheet] {
  const doc: Stylesheet = {
    type: "Stylesheet",
    name,
    uniqueId: id ?? uuidv4(),
    source,
    sourceIsOpen: false,
    layout: { type: "None" },
    attachedSourceFileId: null,
    inspectorPoints: new Set()
  }

  return [documents.concat(doc), doc];
}

/* ensureStylesheetsAreAttached assigns the attachedSourceFileId of each
 * stylesheet if it is null and there is at least one open sourceFile. */
function ensureStylesheetsAreAttached(documents: Document[]): Document[] {
  const openSourceFiles = documents.reduce((set, doc) => {
    if(doc.type === "SourceFile") {
      return set.add(doc.uniqueId);
    }
    return set;
  }, new Set());

  const firstSourceFile = documents.find(doc => doc.type === "SourceFile");

  return documents.map(doc => {
    if(doc.type === "Stylesheet" && (doc.attachedSourceFileId === null || !openSourceFiles.has(doc.attachedSourceFileId))) {
      if(firstSourceFile === undefined) {
        return { ...doc, attachedSourceFileId: null };
      } else {
        return { ...doc, attachedSourceFileId: firstSourceFile.uniqueId };
      }
    }
    return doc;
  });
}

function applyStyle(
  sourceFile: SourceFile,
  stylesheet: Stylesheet,
  onResolved: (stylesheetId: string, layout: Types.LayoutData) => void,
  onRejected: (stylesheetId: string, err: string) => void,
  documents: Document[]): Document[] {
    return setStylesheetLayout(stylesheet.uniqueId, {
      type: "Loading",
      layout: requestLayout(sourceFile.source, stylesheet.source)
        .then(layout => onResolved(stylesheet.uniqueId, layout))
        .catch(err => onRejected(stylesheet.uniqueId, err))
    }, documents);
  }

function applyStylesheet(
  uniqueId: string,
  onResolved: (stylesheetId: string, layout: Types.LayoutData) => void,
  onRejected: (stylesheetId: string, err: string) => void,
  documents: Document[]): Document[] {
    const stylesheet = findDocumentById(uniqueId, documents);
    assert(stylesheet && stylesheet.type === "Stylesheet");

    if(stylesheet.attachedSourceFileId === null) {
      return setStylesheetLayout(stylesheet.uniqueId,
        { type: "Error", error: "No attached source file!" }, documents);
    }

    const attachedSourceFile = findDocumentById(stylesheet.attachedSourceFileId, documents);
    if(attachedSourceFile === null) {
      return setStylesheetLayout(stylesheet.uniqueId,
        { type: "Error", error: "Source file is not open!" }, documents);
    }

    assert(attachedSourceFile.type === "SourceFile");
    return applyStyle(attachedSourceFile, stylesheet, onResolved, onRejected, documents);
}

function findDocumentById(uniqueId: string, documents: Document[]): Document | null {
  for(const doc of documents) {
    if(doc.uniqueId === uniqueId) return doc;
  }
  return null;
}

export default function CustomLayoutViewer() : JSX.Element {
  const [viewModel,      setViewModel]      = useState<MP.Model>(MP.newModel());
  const [documents,      setDocuments]      = useState<Document[]>([]);
  const [tabContextMenu, setTabContextMenu] = useState<ContextMenuState>(null);
  const [modalDialog,    setModalDialog]    = useState<ModalDialogProps | null>(null);
  const [manifest,       setManifest]       = useState<Types.DocumentMetadata[]>([]);

  function updateManifest() {
    reqManifest().then(manifest => {
      setManifest(manifest);
    });
  }

  useEffect(updateManifest, []);

  useEffect(() => {
    function onKeyDown(event: KeyboardEvent) {
      if(event.key === "Escape") {
        /* Clear inspector points for all stylesheets. */
        setDocuments(docs => docs.map(doc => {
          if(doc.type === "Stylesheet") {
            return { ...doc, inspectorPoints: new Set() };
          } else {
            return doc;
          }
        }));
      }
    }

    window.addEventListener("keydown", onKeyDown);
    return () => {
      window.removeEventListener("keydown", onKeyDown);
    }
  }, []);

  const onStyleUpdatesResolved = useCallback((stylesheetId: string, layoutData: Types.LayoutData) => {
    setDocuments(docs => modifyDocument(stylesheetId, (doc) => {
      assert(doc.type === "Stylesheet");
      if(doc.layout.type === "Loading") {
        return {
          ...doc,
          layout: { type: "Success", layout: layoutData }
        }
      } else {
        return doc;
      }
    }, docs));
  }, []);

  const onStyleUpdatesRejected = useCallback((stylesheetId: string, err: string) => {
    setDocuments(docs => modifyDocument(stylesheetId, (doc) => {
      assert(doc.type === "Stylesheet");
      if(doc.layout.type === "Loading") {
        return {
          ...doc,
          layout: { type: "Error", error: err }
        }
      } else {
        return doc;
      }
    }, docs));
  }, []);

  function updateStylesheetSource(uniqueId: string, source: string) {
    setDocuments(docs => setStylesheetSource(uniqueId, source, docs));
  }

  function updateStylesheetAttachedSourceFile(uniqueId: string, srcId: string) {
    setDocuments(docs => setStylesheetAttachedSourceFile(uniqueId, srcId === "" ? null : srcId, docs));
  }

  function updateSourceFileSource(uniqueId: string, source: string) {
    function onEditingFinished() {
      setDocuments(docs => {
        const attachedStylesheets = docs.filter(doc =>
          doc.type === "Stylesheet" && doc.attachedSourceFileId === uniqueId);

        let docsMod = docs;
        for(const stylesheet of attachedStylesheets) {
          docsMod = applyStylesheet(stylesheet.uniqueId, onStyleUpdatesResolved, onStyleUpdatesRejected, docsMod);
        };

        return docsMod;
      });
    }

    const t = setTimeout(onEditingFinished, 1000);
    setDocuments(docs => {
      /* Update document source */
      const docs1 = setSourceFileSource(uniqueId, source, docs)

      /* If the document hasn't been edited in over a second, update
       * all attached stylesheets */
      const docs2 = modifyDocument(uniqueId, doc => {
        if(doc === null) return doc;
        assert(doc.type === "SourceFile");

        if(doc.lastEditTimer !== null) {
          clearTimeout(doc.lastEditTimer);
        }

        return {
          ...doc,
          lastEditTimer: t
        };
      }, docs1);

      return docs2;
    });
  }

  function newStylesheet(name: string, source: string=defaultStyle, id: string | undefined=undefined) {
    const [newDocs, doc] = addNewStylesheet(name, source, id, documents);
    /* Add the stylesheet and attempt to attach a source file */
    setDocuments(ensureStylesheetsAreAttached(newDocs));
    /* Attempt to apply stylesheets when they are first loaded */
    setDocuments(docs => applyStylesheet(doc.uniqueId, onStyleUpdatesResolved, onStyleUpdatesRejected, docs))
    setViewModel(model => MP.selectChild(doc.uniqueId, MP.addChild(doc.uniqueId, model)));
  }

  function newSourceFile(name: string, source: string=defaultSource, id: string | undefined=undefined) {
    const [newDocs, doc] = addNewSourceFile(name, source, id, documents);
    /* Add the source file and attempt to attach source files to stylesheets */
    setDocuments(ensureStylesheetsAreAttached(newDocs));
    setViewModel(model => MP.selectChild(doc.uniqueId, MP.addChild(doc.uniqueId, model)));
  }

  function openTabContextMenu(clientX: number, clientY: number, childId: string) {
    const menu: CM.MenuTreeSubmenu = {
      type: "Submenu",
      label: "",
      items: [
        {
          type: "Entry",
          label: "Save",
          action: () => {
            const doc = findDocumentById(childId, documents);
            if(!doc) return;
            reqDocumentWrite(doc.uniqueId, doc.type, doc.name, doc.source).then(() => {
              updateManifest();
            });
          }
        },
        {
          type: "Entry",
          label: "Rename",
          action: () => {
            const documentName = findDocumentById(childId, documents)?.name;
            setModalDialog({
              message: "Rename \"" + documentName + "\" to:",
              buttonLabel: "Rename",
              requireResponse: true,
              onDone: (response: string | null) => {
                setModalDialog(null);
                if(response === null) return;

                /* Rename locally */
                setDocuments(docs => docs.map(doc => {
                  if(doc.uniqueId === childId) {
                    return { ...doc, name: response };
                  } else {
                    return doc;
                  }
                }));

                /* Rename on the server */
                reqDocumentRename(childId, response).then(() => {
                  updateManifest();
                });
              },
              onCancel: () => {
                setModalDialog(null);
              }
            });
          }
        },
        {
          type: "Entry",
          label: "Close",
          action: () => {
            setDocuments(docs => closeDocument(childId, docs));
            setViewModel(model => MP.removeChild(childId, model));
          }
        },
        {
          type: "Entry",
          label: "Delete",
          action: () => {
            const documentName = findDocumentById(childId, documents)?.name;
            setModalDialog({
              message: "Are you sure you want to delete \"" + documentName + "\"?",
              buttonLabel: "Yes",
              requireResponse: false,
              onDone: () => {
                setModalDialog(null);

                /* Close the document */
                setDocuments(docs => closeDocument(childId, docs));
                setViewModel(model => MP.removeChild(childId, model));

                /* Request that the document be deleted on the server */
                reqDocumentDelete(childId).then(() => {
                  updateManifest();
                });
              },
              onCancel: () => {
                setModalDialog(null);
              }
            });
          }
        }
      ]
    }

    setTabContextMenu({ clientX, clientY, menu });
  }

  function onNodeClicked(documentId: string, nodeId: number, event: React.MouseEvent) {
    if(event.shiftKey) {
      setDocuments(docs => addStylesheetInspectorPoint(documentId, nodeId, docs))
    } else {
      setDocuments(docs => setStylesheetInspectorPoint(documentId, nodeId, docs))
    }
  }

  const documentViews: Array<React.ReactElement<MP.ViewProps>> =
    documents.map(doc => {
      if(doc.type === "Stylesheet") {
        let result: JSX.Element = <></>;
        switch(doc.layout.type) {
        case "Loading": {
          result = <span>Loading...</span>;
        } break;
        case "Success": {
          result = <>
                     <StylishTextView
                       layout={doc.layout.layout}
                       onClick={(nodeId: number, event: React.MouseEvent) => onNodeClicked(doc.uniqueId, nodeId, event)}
                       inspectorIds={doc.inspectorPoints}/>
                     <Inspector
                       ids={doc.inspectorPoints}
                       layout={doc.layout.layout}
                       onClick={(nodeId: number, event: React.MouseEvent) => onNodeClicked(doc.uniqueId, nodeId, event)}/>
                   </>;
        } break;
        case "Error": {
          result = <pre>{ doc.layout.error }</pre>;
        } break;
        }

        const srcFileOptions: JSX.Element[] = documents.filter(doc => doc.type === "SourceFile")
          .map(srcDoc => {
            assert(srcDoc.type === "SourceFile");
            return <option
                     key={ srcDoc.uniqueId }
                     value={ srcDoc.uniqueId }>
                     { srcDoc.name }
                   </option>
          });

        return <View key={doc.uniqueId} id={doc.uniqueId} label={doc.name}>
                 <div>
                   <div>
                     <Button onClick={() =>
                       setDocuments(docs => toggleStylesheetSourceIsOpen(doc.uniqueId, docs))}>
                       { doc.sourceIsOpen ? "Close Styles" : "View Styles" }
                     </Button>
                     <Button onClick={() =>
                       setDocuments(docs => applyStylesheet(doc.uniqueId, onStyleUpdatesResolved, onStyleUpdatesRejected, docs))}>
                       Apply Stylesheet
                     </Button>
                     <span style={{fontFamily: "sans-serif", fontSize: "0.7em", padding: "4px"}}>to</span>
                     <select
                       onChange={(e) => updateStylesheetAttachedSourceFile(doc.uniqueId, e.target.value)}
                       value={doc.attachedSourceFileId === null ? "" : doc.attachedSourceFileId} >
                       { srcFileOptions }
                     </select>
                   </div>
                   { doc.sourceIsOpen ?
                     <CodeMirror
                       value={doc.source}
                       onChange={newSource => updateStylesheetSource(doc.uniqueId, newSource)}/>
                     : <></>
                   }
                   { result }
                 </div>
               </View>
      } else {
        return <View key={doc.uniqueId} id={doc.uniqueId} label={doc.name}>
                 <CodeMirror
                   value={doc.source}
                   onChange={newSource => updateSourceFileSource(doc.uniqueId, newSource)}/>
               </View>
      }
    });

  const menus: CM.MenuTreeSubmenu[] = [
    {
      type: "Submenu",
      label: "New",
      items: [
        {
          type: "Entry",
          label: "New Source File",
          iconSrc: require("./static/haskell.svg"),
          action: () => {
            setModalDialog({
              message: "Source file name:",
              buttonLabel: "Create",
              requireResponse: true,
              onDone: (response: string | null) => {
                newSourceFile(response ?? "Untitled");
                setModalDialog(null);
              },
              onCancel: () => {
                setModalDialog(null);
              }
            });
          }
        },
        {
          type: "Entry",
          label: "New Stylesheet",
          iconSrc: require("./static/hass.svg"),
          action: () => {
            setModalDialog({
              message: "Stylesheet name:",
              buttonLabel: "Create",
              requireResponse: true,
              onDone: (response: string | null) => {
                newStylesheet(response ?? "Untitled");
                setModalDialog(null);
              },
              onCancel: () => {
                setModalDialog(null);
              }
            });
          }
        }
      ]
    },
    {
      type: "Submenu",
      label: "Load",
      items: manifest.map(metadata => ({
        type: "Entry",
        label: metadata.name,
        iconSrc: metadata.role === "Stylesheet"
          ? require("./static/hass.svg")
          : require("./static/haskell.svg"),
        action: () => {
          reqDocumentRead(metadata.id).then(data => {
            /* Check that the document isn't already loaded. */
            if(!documents.find(doc => doc.uniqueId === data.id)) {
              if(metadata.role === "SourceFile") {
                newSourceFile(metadata.name, data.contents, data.id);
              } else {
                newStylesheet(metadata.name, data.contents, data.id);
              }
            } else {
              /* If the document was already loaded, select it */
              setViewModel(model => MP.selectChild(data.id, model));
            }
          });
        }
      }))
    }
  ];

  return <div className={styles.custom_layout_viewer_root}>
           <MB.MenuBar menus={menus}/>
           <div className={styles.workspace_root}>
             <MultiPanelView
               model={viewModel}
               onModelChanged={setViewModel}
               onContextMenu={openTabContextMenu}>
               { documentViews }
             </MultiPanelView>
           </div>
           {
             tabContextMenu === null ? <></>
               : <CM.ContextMenu
                   x={tabContextMenu.clientX}
                   y={tabContextMenu.clientY}
                   onShouldClose={() => setTabContextMenu(null)}
                   menu={tabContextMenu.menu}/>
           }
           {
             modalDialog === null ? <></>
               : <ModalDialog
                   message={modalDialog.message}
                   buttonLabel={modalDialog.buttonLabel}
                   requireResponse={modalDialog.requireResponse}
                   onCancel={modalDialog.onCancel}
                   onDone={modalDialog.onDone} />
           }
         </div>;
}

function reqManifest(): Promise<Types.DocumentMetadata[]> {
  const msg: Types.RequestDocumentManifest = {
    op: "RequestDocumentManifest",
  }
  return new Promise((resolve, reject) => {
    docTransaction(msg).then(res => {
      assert(res.op === "DocumentManifest");
      resolve(res.manifest);
    }).catch(reject);
  });
}

function reqDocumentWrite(id: string, role: Types.DocumentRole, name: string, contents: string): Promise<void> {
  const msg: Types.RequestDocumentWrite = {
    op: "RequestDocumentWrite",
    id, role, name, contents
  }
  return new Promise((resolve, reject) => {
    docTransaction(msg).then(res => {
      assert(res.op === "DocumentSuccess");
      resolve();
    }).catch(reject);
  });
}

function reqDocumentRead(id: string): Promise<Types.DocumentContents> {
  const msg: Types.RequestDocumentRead = {
    op: "RequestDocumentRead", id
  }
  return new Promise((resolve, reject) => {
    docTransaction(msg).then(res => {
      assert(res.op === "DocumentContents");
      resolve(res);
    }).catch(reject);
  });
}

function reqDocumentDelete(id: string): Promise<Types.DocumentSuccess> {
  const msg: Types.RequestDocumentDelete = {
    op: "RequestDocumentDelete", id
  }
  return new Promise((resolve, reject) => {
    docTransaction(msg).then(res => {
      assert(res.op === "DocumentSuccess");
      resolve(res);
    }).catch(reject);
  });
}

function reqDocumentRename(id: string, newName: string): Promise<Types.DocumentSuccess> {
  const msg: Types.RequestDocumentRename = {
    op: "RequestDocumentRename", id, newName
  }
  return new Promise((resolve, reject) => {
    docTransaction(msg).then(res => {
      assert(res.op === "DocumentSuccess");
      resolve(res);
    }).catch(reject);
  });
}

/* DocRes is a type-level function that returns the correct
 * type of response for each type of request */
type DocRes<T extends Types.DocReq> =
  T extends Types.RequestDocumentManifest ? Types.DocumentManifest :
  T extends Types.RequestDocumentWrite    ? Types.DocumentSuccess  :
  T extends Types.RequestDocumentRead     ? Types.DocumentContents :
  T extends Types.RequestDocumentDelete   ? Types.DocumentSuccess  :
  T extends Types.RequestDocumentRename   ? Types.DocumentSuccess  :
  never;

function docTransaction<ReqT extends Types.DocReq>(msg: ReqT): Promise<DocRes<ReqT>> {
  return new Promise((resolve, reject) => {
    const ws = new WebSocket('ws://localhost:1234/');
    ws.onopen = () => {
      ws.send(JSON.stringify(msg));
    };

    ws.onmessage = (event) => {
      const data = JSON.parse(event.data) as DocRes<ReqT> | Types.Error;
      if(data.op === "Error") {
        reject(data.message);
      } else {
        resolve(data);
      }
    }

    ws.onerror = (err) => {
      reject(err);
    }
  });
}

function requestLayout(programSrc:    string, stylesheetSrc: string): Promise<Types.LayoutData> {
  return new Promise((resolve, reject) => {
    const ws = new WebSocket('ws://localhost:1234/');
    ws.onopen = () => {
      ws.send(JSON.stringify({
        op: "RequestCustomLayout",
        stylesheetSrc,
        programSrc
      }));
    };

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
          resolve(data.data);
        } break;
        case "Error": {
          reject(data.message);
        }
      }
    }

    ws.onerror = (_event) => {
      reject("websocket error");
    }
  });
}
