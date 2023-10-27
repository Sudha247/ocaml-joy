# Welcome to Ocaml-Joy
Thank you for your interest in ocaml-joy! This document includes installation instructions and guidelines for contributors.

## Installation

Follow these simple steps to begin working with ocaml-joy :

### 1. Install OCaml

Start by installing OCaml by following the official installation tutorial at [https://ocaml.org/docs/installing-ocaml](https://ocaml.org/docs/installing-ocaml).

### 2. Install Opam

After successfully installing OCaml, proceed to install Opam, the OCaml package manager. It provides a convenient way to install, manage, and share OCaml libraries and tools.

For Windows, Opam can be installed on Windows using Windows Subsystem for Linux (WSL) or other virtualization methods, as OCaml development is primarily done on Unix-like systems. You can set up WSL and then follow the Linux installation instructions.

For Linux, If you're using Debian or Ubuntu, you can install Opam via the system package manager. Open a terminal and run the following commands:

`sudo apt update
sudo apt install opam
`

### 3. Update Opam
Once Opam is installed, make sure to update and upgrade it to the latest version of packages by running the following commands:

`opam update && opam upgrade`

### 4. Install dune
Dune is a build system we are using for our OCaml project. It is to help us to simplify and automate the build process. Run the following command for it to be installed.

`opam install dune`

### 5. Installing Graphics Library:
To install the necessary Graphics library, execute the following command inside the directory where your OCaml code is located:

`opam install graphics`

The Graphics library is a simple graphics module in OCaml that provides functions for creating graphical windows, drawing shapes, and handling user input events.

### 6. Building and Running Examples:
To build and execute examples, navigate to the directory where your OCaml code is located (e.g., ocaml-joy)

`cd ocaml-joy # Change to your project directory`

Then, build the examples using Dune 

`dune build examples/`

Run your preferred file for example to display the file in the examples directory named circle.ml we use the command

`dune exec -- examples/circle.exe`

To verify the success of this installation, you will see the following output:

![circle Image](https://github.com/joanita-51/ocaml-joy/assets/82649346/87bf01ad-836f-4491-97c2-8724b8047429)


## Contribution

We warmly welcome contributions from the community. If you'd like to contribute to Ocaml-Joy after setting it up on your machine, follow these steps to get started:

1. **Select a Good-First Issue:** Begin your contribution journey by checking our issue tracker for issues tagged as 'good-first-issue.' These are typically beginner-friendly tasks designed to help new contributors get acquainted with the project.

2. **Fork the Repository:** Once you've chosen an issue to work on, fork the Ocaml-Joy repository to create your own copy.

3. **Create a Branch:** Before making any changes, create a new branch for your work. This helps keep your changes isolated and organized.

4. **Make Your Changes:** Dive into the code and make the necessary modifications to address the chosen issue. Ensure that you follow our coding guidelines and best practices.

5. **Test Your Changes:** After implementing your modifications, thoroughly test your code to ensure it functions as expected and doesn't introduce new issues.

6. **Submit a Pull Request:** When you're confident that your changes are ready, submit a pull request (PR) to the main repository. Provide a clear description of your changes and reference the issue you've resolved.

7. **Engage in Discussion:** Engage in discussions and reviews on your PR. Our team and the community will provide feedback and guidance to ensure your contribution aligns with the project's goals.

8. **Get Your Contribution Merged:** Once your PR is approved and passes all checks, it will be merged into the main project. Congratulations on your successful contribution!

By following these steps, you'll be actively contributing to Ocaml-Joy and helping improve the project. We appreciate your consideration of joining our community!
