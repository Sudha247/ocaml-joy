# Joy in OCaml

## Table of Contents
- [Project Overview](#project-overview)
- [Project Benefits](#project-benefits)
- [Installation](#installation)
- [Getting Help](#getting-help)
- [Other wonderful Shapes](#other-wonderful-shapes)
- [Contribution](#contribution)
- [Acknowlegments](#acknowlegments)

## Project Overview
This library is like a toolbox for creating cool shapes and patterns using OCaml. It takes inspiration from [joy](https://github.com/fossunited/joy). Whether you're an artist or a coder, you can use it to make interesting art with code.

## Project Benefits

Here's how this project can make your life easier and more creative:

1. **Get Creative with Geometry:** This project simplifies geometric creative coding in OCaml, helping you explore the exciting world where programming meets art.

2. **User-Friendly Tools:** We provide an easy-to-use API, so whether you're a newbie or a pro, you can dive into creative coding without confusion.

3. **Basic Shapes at Your Fingertips:** Use simple shapes as your creative building blocks, making it a breeze to start your artistic coding journey.

4. **Mix and Match with Ease:** Combine, transform, and build complex shapes effortlessly.


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

## Getting Help
At any point, please don't hesitate to ask questions. You can contact the mentors either here in the issue tracker or in the #outreachy channel in [OCaml discord](https://discord.com/invite/cCYQbqN). 

## Shape Examples
![axes image](https://github.com/joanita-51/ocaml-joy/assets/82649346/0b8b402e-65ee-45a8-b568-cf0434a10f5e)
![circle graph image](https://github.com/joanita-51/ocaml-joy/assets/82649346/1ac34bd9-553b-45e2-90cf-240c73b63256)
![rectangle image](https://github.com/joanita-51/ocaml-joy/assets/82649346/c00e5df4-83bd-4cf7-a864-6f26da0d1fac)
![circle grid image](https://github.com/joanita-51/ocaml-joy/assets/82649346/20176f2d-cf96-4ec1-93ed-e5eaf1682600)
[star image](https://github.com/joanita-51/ocaml-joy/assets/82649346/5a8cd0ea-f00b-46b0-b34d-406d76561a2d)
![high order transformation image](https://github.com/joanita-51/ocaml-joy/assets/82649346/eeb2d2fc-86c5-4159-b559-4e280232798f)
<!-- ![star image](https://github.com/joanita-51/ocaml-joy/assets/82649346/5a8cd0ea-f00b-46b0-b34d-406d76561a2d)
![circle rectangle image](https://github.com/joanita-51/ocaml-joy/assets/82649346/d53af149-83e3-40ac-b34d-d83c69eede3b)
![circle row joy image](https://github.com/joanita-51/ocaml-joy/assets/82649346/6a67187a-ef85-4549-9920-a674740731a7)
![triangle image](https://github.com/joanita-51/ocaml-joy/assets/82649346/de6f1384-94cd-4d78-8380-74d0c38b9331)
![concentric circles image](https://github.com/joanita-51/ocaml-joy/assets/82649346/8085d796-4672-413f-904e-01d843e076a1)
![Line image](https://github.com/joanita-51/ocaml-joy/assets/82649346/3b53a1b1-4192-4039-a78d-452116cf683b)
![translate circle image](https://github.com/joanita-51/ocaml-joy/assets/82649346/f4b570ef-cc66-46a8-9360-8796e1fb7361)
![translate rectangle image](https://github.com/joanita-51/ocaml-joy/assets/82649346/aedfaeb4-4ddc-4d82-a77f-a2dfdd071b3d)
![polygon image](https://github.com/joanita-51/ocaml-joy/assets/82649346/1201e352-ef3f-433c-82cc-c834b3d52daf)
![high order transformation image](https://github.com/joanita-51/ocaml-joy/assets/82649346/eeb2d2fc-86c5-4159-b559-4e280232798f)
![translate eclipse image](https://github.com/joanita-51/ocaml-joy/assets/82649346/70026da8-92a3-4c3f-a90a-6827b87e1b8a)
![circle grid image](https://github.com/joanita-51/ocaml-joy/assets/82649346/2d24f4a5-2d06-4091-806a-1c7b412a3ef7) -->


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


## Acknowlegments

*This library is inspired by [joy](https://github.com/fossunited/joy). Thanks to the creators!*
