<h1 align="center">
  <img src="./logo.png"/><br>
  Create-Haskstar-App 
</h1>

## Currently WIP. Contributions highly welcome!

## tl;dr
Automatically build and deploy fullstack haskell webapps in one command!
Batteries included, even devops!

Default stack is Haskell/Elm/Postgres, but see roadmap for upcoming options



## Why?

Designed to automate all the redundant, not-so-fun parts of setting up a stack.
This is almost like a framework - monolithic and highly opinionated - 
designed to largely get production ready haskell-based apps up and running quickly!

As all monolithic frameworks come with certain drawbacks (too much magic, difference of opinions on the "right way", etc)
the eventual goal of this is to make the full setup process highly customizable.


## Setup

--TODO: Work on nix build

SIf you are on OSX, simply run the following and follow the prompts!
```bash
git clone git@github.com:smaccoun/create-haskstar-app.git
cd create-haskstar-app

chmod +x create-app
./create-app my-app
```

### Manual

To manually build
```bash
./build.sh   # Runs stack build and moves binary to local dir

chmod +x create-app
./create-app my-app
```


# Roadmap


### Deployment 

|   Type   | Status   |
|----------|:---------|
| OSX Binary     |   ✅     |
| Homebrew | In progress   |
| Nix      |      |
| ....     |      |

### ---------Dev Ops-----------

|   Platforms | Status   |
|----------|:---------|
| CircleCI |          |
| AWS      |   In progress    |
| GCloud   |      |


### ---------Front-End-----------

#### Frameworks
|   lib    | Status   |
|----------|:---------|
| Elm      |   ✅ 
| [Miso](https://github.com/dmjio/miso)  |      |
| Reflex? (haskell)    |          |
| Halogen (purescript)    |          |


#### User/Session/Auth

|   Feature | Status   |
|----------|:---------|
| Login/Register view |          |
| password hasing (scrypt) |      |
| OAuth     |      |
| Cookie    |      |

#### Styling

|   Feature | Status   |
|----------|:---------|
| Bulma |          |
| Font Awesome |      |
| Ionic |      |

### ---------Back-End-----------

|   lib    | Status   |
|----------|:---------|
| [Servant](https://hackage.haskell.org/package/servant)  |   ✅ 
| [Selda](https://selda.link/)   |   in progress
| yesod?    |          |


### ---------DB-----------

**Only plan on supporting postgres for a while**

|   lib    | Status   |
|----------|:---------|
| Rails migration |      |
| Persistent? |          |

