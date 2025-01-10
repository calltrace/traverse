
# Reference DDLog Project

Serves as a **reference implementation of a DDLog project**. It provides a baseline for **implementing code generation** by defining the structure, relations, and logic required for generating output artifacts such as source code, diagrams, or transformed ASTs.

> **Note**: This is a work-in-progress project used for learning and demonstration purposes.

## Generating the Solidity DDLog Project

To generate a DDLog project for Solidity, ensure the following are installed:
1. **DDLog Compiler**: Install the [DDLog compiler](https://github.com/vmware/differential-datalog).
2. **Rust**: Required for building the generated Rust runtime.

### Steps

1. **Prepare the Solidity Logic File**:
   - The file `specs/solidity.dl` contains the Datalog rules and relations for processing Solidity contracts.

2. **Run the DDLog Compiler**:
   - Generate the DDLog project by running the following command:
     ```bash
     ddlog -i solidity.dl -o solidity_ddlog
     ```
   - **Options**:
     - `-i`: Path to the input `.dl` file (e.g., `solidity.dl`).
     - `-o`: Directory where the DDLog project will be generated (e.g., `solidity_ddlog`).

3. **Build the Generated Rust Code**:
   - Navigate to the generated project directory:
     ```bash
     cd solidity_ddlog
     ```
   - Build the project using Cargo:
     ```bash
     cargo +1.76 build --release
     ```

4. **Run the Project**:
   - Use the built runtime to process input data (e.g., Solidity contract representations).

