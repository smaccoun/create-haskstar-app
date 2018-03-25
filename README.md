<h1 align="center">
  <img src="./logo.png"/><br>
  Create-Haskstar-App 
</h1>

## tl;dr
Automatically build and deploy fullstack haskell webapps in one command!
Batteries included, even devops!

Default stack is Haskell/Elm/Postgres, but see roadmap for upcoming options

#### Currently wip. Contributions highly welcome!

## Why?

Designed to automate all the redundant, not-so-fun parts of setting up a stack.
This is almost like a framework, monolithic and highly opinionated,
designed to largely get production ready haskell-based apps up and running quickly!

As all monolithic frameworks come with certain drawbacks (too much magic, difference of opinions on the "right way", etc)
the eventual goal of this is to make the full setup process highly customizable.


## Setup

Simply run the following and follow the prompts!
```bash
$ git clone git@github.com:smaccoun/create-haskstar-app.git

$ cd create-haskstar-app

$ ./create-app
```


# Roadmap

### DevOps

|   lib    | Status   |
|----------|:---------|
| AWS      |   In progress    |
| GCloud   |      |


### Front-End Options

|   lib    | Status   |
|----------|:---------|
| Elm      |   ✅ 
| Miso (haskell)  |      |
| Reflex? (haskell)    |          |
| Halogen (purescript)    |          |

### Back-End

|   lib    | Status   |
|----------|:---------|
| servant  |   ✅ 
| selda    |   in progress
| yesod?    |          |


### DB

|   lib    | Status   |
|----------|:---------|
| Rails migration |      |


