# ACE — ABAP Code Explorer

## Project Overview

ACE is an interactive ABAP source code analysis and visualization tool for SAP systems. It allows developers to explore program/class/function/OData service code flows without debugging, trace data dependencies, and generate Mermaid diagrams.

## Repository Structure

```
/src/           # All ABAP source files (.abap + .xml metadata pairs)
.abapgit.xml    # abapGit configuration (deploys via abapGit to SAP system)
```

## Key Objects

| Object | Role |
|--------|------|
| `z_ace.prog.abap` | Entry point — selection screen |
| `z_ace_standalone.prog.abap` | Monolithic single-file deployment version |
| `zcl_ace.clas.abap` | Main orchestrator class |
| `zcl_ace_source_parser.clas.abap` | Core ABAP syntax parser |
| `zcl_ace_parser.clas.abap` | Coordinates parsing and flow analysis |
| `zcl_ace_tree_builder.clas.abap` | Builds hierarchical navigation trees |
| `zcl_ace_window.clas.abap` | Multi-window UI management |
| `zcl_ace_mermaid.clas.abap` | Mermaid diagram generation |
| `zcl_ace_metrics.clas.abap` | Code complexity metrics |
| `zif_ace_parse_data.intf.abap` | Central type contract interface |

### Specialized Parsers (delegates of `zcl_ace_source_parser`)

- `zcl_ace_parse_calls.clas.abap` — method/function call extraction
- `zcl_ace_parse_calcs.clas.abap` — variable calculation tracking
- `zcl_ace_parse_vars.clas.abap` — variable declarations and scope
- `zcl_ace_parse_params.clas.abap` — parameter extraction
- `zcl_ace_parse_handlers.clas.abap` — event handler registration
- `zcl_ace_parse_events.clas.abap` — event definitions

## Development Workflow

- Code is maintained in this Git repo and deployed to SAP via **abapGit**
- The standalone file `z_ace_standalone.prog.abap` is a merged single-file version for easy deployment
- No CI/CD pipeline; testing is done manually via `zace_scan_test.prog.abap` and `z_calc_demo.prog.abap`

## Architecture

```
Selection Screen (z_ace.prog)
  └── ZCL_ACE (orchestrator)
        ├── ZCL_ACE_SOURCE_PARSER
        │     ├── ZCL_ACE_PARSE_CALLS
        │     ├── ZCL_ACE_PARSE_CALCS
        │     ├── ZCL_ACE_PARSE_VARS
        │     └── ZCL_ACE_PARSE_PARAMS
        ├── ZCL_ACE_TREE_BUILDER
        └── ZCL_ACE_WINDOW (multi-window UI)
              ├── ZCL_ACE_MERMAID (diagrams)
              ├── ZCL_ACE_METRICS (complexity)
              └── ZCL_ACE_TABLE_VIEWER (ALV)
```

## Notes

- Language: ABAP (SAP)
- All types are defined in `zif_ace_parse_data` — check this interface first when working with data structures
- Uses SAP GUI containers (ALV, HTML viewer, split containers) for UI
- RTTI-based reflection is handled by `zcl_ace_rtti` and `zcl_ace_rtti_tree`
