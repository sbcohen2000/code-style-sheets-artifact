/* Layout and Measurement */

export interface Measure {
  op: "Measure";
  spec: MeasureSpec;
}

export type Styles = Array<[string, string]>;

export interface MeasureText {
  op: "MeasureText";
  text: string;
  styles: Styles;
}

export interface MeasureHtml {
  op: "MeasureHtml";
  contents: string;
  styles: Styles;
}

export type MeasureSpec = MeasureText | MeasureHtml;

export interface RequestLeading {
  op: "RequestLeading";
}

export interface LayoutDone {
  op: "LayoutDone";
  data: LayoutData;
  description: string;
}

export interface Error {
  op: "Error";
  message: string;
}

export interface Info {
  op: "Info";
  type: "examples" | "customLayout";
  nExamples?: number;
}

export type LayoutMsg = Measure | RequestLeading | LayoutDone | Error | Info;

export interface LayoutData {
  metrics: LayoutMetrics;
  fragments: Fragments;
  root: FTTerm;
}

export interface FTTerm {
  children: FTTerm[];
  style: ComputedStyle;
  id: number;
  lines: LineNumberSpan;
  fragments: FragmentSpan;
}

export interface ComputedStyle {
  padding: number;
  margin: number;
  borderWidth: number;
  borderColor: string;
  backgroundColor: string;
  borderRadius: number;
  otherStyles: Styles;
  inspectorInfo: InspectorInfo;
}

export interface InspectorInfo {
  path: number[] | null;
  classes: string[];
}

export interface LineNumberSpan {
  begin: number;
  end: number;
}

export interface FragmentSpan {
  begin: number;
  end: number;
}

export interface FlipSpan {
  begin: number;
  end: number;
  flip: Boolean;
}

export type Fragments = [Gadget[], Fragment, Gadget[]][]

export interface GShim {
  type: "GShim";
  id: number;
  width: number;
}

export interface GAdjustableShim {
  type: "GAdjustableShim"
  id: number;
}

export type Gadget = GShim | GAdjustableShim;

export interface FNewline {
  type: "FNewline";
  id: number;
}

export interface FText {
  type: "FText";
  id: number;
  text: String;
  size: Size;
}

export interface FHtml {
  type: "FHtml";
  id: number;
  contents: String;
  size: Size;
}

export type Fragment = FNewline | FText | FHtml;

export interface Size {
  width: number;
  height: number;
}

export interface LayoutMetrics {
  interLineSpacing: LineMetric[];
  drawOrder: Array<[number, DrawOrder[]]>;
}

export interface LineMetric {
  top: number;
  aboveSpace: number;
  lineHeight: number;
  belowSpace: number;
}

export interface HorzLine {
  type: "HorzLine";
  line: number;
  side: Side;
  offset: number;
  span: FlipSpan;
}

export interface ClosePath {
  type: "ClosePath";
}

export type DrawOrder = HorzLine | ClosePath;

export type Side = "above" | "below"

/* File System and Documents */

export type DocumentRole = "Stylesheet" | "SourceFile";

export interface DocumentMetadata {
  id: string;
  role: DocumentRole;
  name: string;
}

export interface RequestDocumentManifest {
  op: "RequestDocumentManifest";
}

export interface RequestDocumentBase {
  op: string;
  id: string;
}

export interface RequestDocumentWrite extends RequestDocumentBase {
  op: "RequestDocumentWrite";
  role: DocumentRole;
  name: string;
  contents: string;
}

export interface RequestDocumentRead extends RequestDocumentBase {
  op: "RequestDocumentRead";
}

export interface RequestDocumentDelete extends RequestDocumentBase {
  op: "RequestDocumentDelete";
}

export interface RequestDocumentRename extends RequestDocumentBase {
  op: "RequestDocumentRename";
  newName: string;
}

export interface DocumentSuccess {
  op: "DocumentSuccess";
}

export interface DocumentContents {
  op: "DocumentContents";
  id: string;
  contents: string;
}

export interface DocumentManifest {
  op: "DocumentManifest";
  manifest: DocumentMetadata[];
}

export type DocReq = RequestDocumentManifest
  | RequestDocumentWrite
  | RequestDocumentRead
  | RequestDocumentDelete
  | RequestDocumentRename;

export type DocMsg = DocumentContents
  | DocumentManifest
  | Error
  | DocumentSuccess;
