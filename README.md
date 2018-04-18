<h1 align="center">
  <img src="./logo.png"/><br>
  Create-Haskstar-App 
</h1>



## tl;dr


Automatically develop, test, and deploy full-stack haskell WebApps with one command line util!

```bash
hasm new example-app

cd example-app

hasm start db
hasm start front-end
hasm start back-end

hasm run migrations
```

The name HaskStar (Hask*) implies building a WebApp with Haskell as a back-end and your choice of front-end, devops, etc.

(Default stack is Haskell/Elm/Postgres, but see roadmap for upcoming options)

#### Currently Very WIP, but I will be working on this full time for a while. Contributions highly welcome!


## GOALS

#### Major Goals:

- Quick scaffolding of full-stack apps with Haskell as a back-end for rapid prototyping that can turn into full scale apps
- Simplify and reduce time spent on devOps and deploying to cloud
- Decent level of customization, especially on the front-end
- Provide template of best practices and best libraries for haskell based webApps (and respective front-ends)
- Most batteries included - preset with all the libraries and configuration common to most or all webapps (authentication/login, styling framweworks, forms)
- Solid tooling (e.g. auto generated REST apis from code, hot reloading, etc)


Designed to automate all the redundant, not-so-fun parts of setting up a stack.
This is almost like a framework - monolithic and highly opinionated - 
designed to largely get production ready haskell-based apps up and running quickly!

As all monolithic frameworks come with certain drawbacks (too much magic, difference of opinions on the "right way**, etc**
the eventual goal of this is to make the full setup process highly customizable.


## Setup

--TODO: Work on nix build, homebrew, hackage.


**NOTE** *The first build may take a VERY long time, especially if you do not have lts-9.4*
**After the first time builds should be quite fast! Grab a coffee during the initial setup! **

**Prerequisites**
You must have the following installed

- Docker
- npm
- yarn
- stack

# Setup

```bash
git clone git@github.com:smaccoun/create-haskstar-app.git
cd create-haskstar-app
./install.sh
```

# Development Philosophy

Major pluggable parts (such as Elm or Miso) will each be kept in a separate repo
that haskstar will build from (e.g. see [haskstar-elm](https://github.com/smaccoun/haskstar-elm)). 
That way these can be used also be used as standalone templates

I am working on this full time and will take a very iterative approach. 
Idea is to get stack working for a certain set of configurations, then continually abstract until nearly anything is plug-in-able



# Roadmap

### ---------Front-End-----------

#### Frameworks
|   lib    | Status   |
|----------|:---------|
| Elm      |   ✅ 
| [Miso](https://github.com/dmjio/miso)  | Coming soon |
| Reflex? (haskell)    |          |
| Halogen (purescript)    |          |


#### User/Session/Auth

|   Feature | Status   |
|----------|:---------|
| Login/Register view |     ✅      |
| password hasing (scrypt) |   ✅    |
| OAuth     |      |
| Cookie    |      |

#### Styling

|   Feature | Status   |
|----------|:---------|
| Bulma |     ✅      |
| Font Awesome |      |
| Ionic |      |

### ---------Back-End-----------

#### Libraries
| lib                                                    | Status      |
|--------------------------------------------------------|-------------|
| [Servant](https://hackage.haskell.org/package/servant) | ✅          |
| [Beam](https://tathougies.github.io/beam/)             | ✅          |
| [Selda](https://selda.link/)   |  ?? 
| yesod?                                                 |             |

#### Features
| feature                          | Status      |
|--------------------------------------------------------|-------------|
| Automatic Swagger UI generation  | ✅          |
| Automatic Swagger UI server      | ✅          |




### ---------DB-----------

**Only plan on supporting postgres for a while**

|   lib    | Status   |
|----------|:---------|
| Postgres simple migration |  ✅ |
| Rails migration |          |
| Persistent? |          |


### Distribution

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

