import { useState } from "react";
import "./App.css";
import sunIcon from "./assets/sun.svg";
import moonIcon from "./assets/moon.svg";

function HeaderSection() {
  const [dark, setDark] = useState(
    () => window.matchMedia("(prefers-color-scheme: dark)").matches,
  );

  const toggle = () => {
    const newDark = !dark;
    setDark(newDark);
    document.documentElement.classList.toggle("dark", newDark);
    document.documentElement.classList.toggle("light", !newDark);
  };

  return (
    <div className="header">
      <div>
        <p className="header-title">GCL Workbench</p>
        <p className="header-subtitle">
          Interactive Guarded Command Language Workbench
        </p>
      </div>
      <button
        className="btn darkmode-btn"
        onClick={toggle}
        aria-label="Toggle dark mode">
        <img
          src={dark ? sunIcon : moonIcon}
          width="14"
          height="14"
          alt={dark ? "Switch to light mode" : "Switch to dark mode"}
        />
      </button>
    </div>
  );
}

function InputSection() {
  return (
    <div className="panel input-panel">
      <div className="panel-header">
        <p className="panel-title">Input</p>
      </div>
      <textarea
        className="input-editor"
        spellCheck={false}
        placeholder="Enter GCL program..."
      />
    </div>
  );
}

function OutputSection() {
  return (
    <div className="panel output-panel">
      <div className="panel-header">
        <p className="panel-title">Output</p>
      </div>
      <div className="output-panel-contents">
        <div className="panel-buttons">
          <button className="btn">Parser</button>
          <button className="btn">Compiler</button>
          <button className="btn">AST</button>
          <button className="btn">Program Graph</button>
          <button className="btn">DOT Source</button>
          <button className="btn">Interpreter</button>
          <button className="btn">RISC-V</button>
          <button className="btn">Floyd-Hoare</button>
          <button className="btn">Security Analyser</button>
          <button className="btn">Model Checking</button>
          <button className="btn">Sign Analyser</button>
        </div>
        <p>actual output here</p>
      </div>
    </div>
  );
}

function App() {
  return (
    <>
      <HeaderSection></HeaderSection>
      <div className="main-page">
        <InputSection></InputSection>
        <OutputSection></OutputSection>
      </div>
    </>
  );
}

export default App;
