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

const MODES = ["Parser", "Compiler", "AST", "Program Graph", "DOT Source", "Interpreter", "RISC-V", "Floyd-Hoare", "Security Analyser", "Model Checking", "Sign Analyser"];

function OutputSection() {
  const [selected, setSelected] = useState("Parser");

  return (
    <div className="panel output-panel">
      <div className="panel-header">
        <p className="panel-title">Output</p>
      </div>
      <div className="output-panel-contents">
        <div className="panel-buttons">
          {MODES.map((mode) => (
            <button
              key={mode}
              className={`btn${selected === mode ? " btn-active" : ""}`}
              onClick={() => setSelected(mode)}
            >
              {mode}
            </button>
          ))}
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
