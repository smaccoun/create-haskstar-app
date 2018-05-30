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
hasm start frontend
hasm start backend

hasm run migrations local

hasm configure staging db   ## Configure a remote db instance (e.g. RDS or GCloud SQL)
hasm deploy staging ## Currently requires some manual work with Google Cloud prior 
```

The name HaskStar (Hask*) implies building a WebApp with Haskell as a back-end and your choice of front-end, devops, etc.

Default stack is Haskell/Elm/Postgres, but see roadmap for upcoming options

Current backend: [Beam-Servant](https://github.com/smaccoun/beam-servant)
Current frontend: [Haskstar-Elm](https://github.com/smaccoun/haskstar-elm)

### Currently Very WIP, but I will be working on this full time for a while. Contributions highly welcome!

## Table Of Contents
- [Setup](#setup)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
  - [Deployment](#deployment-setup)
- [Goals](#goals)
- [Roadmap](#roadmap)
   - [Backend](#back-end)
     - [Auth](#auth)
     - [Features](#features)
     - [Libraries](#libraries)
   - [Frontend](#front-end)
     - [Frameworks](#frameworks)
     - [Auth views](#auth-views)
     - [Admin Panel](#admin-panel)
   - [Ops](#ops)
   - [DB](#db)
     - [Migrations](#migrations)
   - [Distribution](#distribution)
- [Development Philosophy](#development-philosophy)




## GOALS

#### Major Goals:

**Development**
- Quick scaffolding of full-stack apps with Haskell as a back-end for rapid prototyping that can turn into full scale apps

- Decent level of customization, especially on the front-end
- Provide template of best practices and best libraries for haskell based webApps (and respective front-ends)
- Most batteries included - preset with all the libraries and configuration common to most or all webapps (authentication/login, styling framweworks, forms)
- Solid tooling (e.g. auto generated REST apis from code, hot reloading, etc)

**Deployment**
- Simplify and reduce time spent on devOps and deploying to cloud
- Highly declarative cloud configurations (achieved with Kubernetes)
- Automate all the common stuff (e.g. automatic TLS provisioning)
- Ability to rollback and forward deployments
- Secure/Scalable/Health monitoring (again...Kubernetes)

In other words.....

Designed to automate all the redundant, not-so-fun parts of setting up a stack.
This is almost like a framework - monolithic and highly opinionated - 
designed to largely get production ready haskell-based apps up and running quickly!

As all monolithic frameworks come with certain drawbacks (too much magic, difference of opinions on the "right way**, etc**
the eventual goal of this is to make the full setup process highly customizable.


## Setup

--TODO: Work on nix build, homebrew, hackage.

### Prerequisites
You must have the following installed for basic local development and setup

- stack
- Docker
- npm
- yarn


### Installation

**Install the command line util (hasm)**

```bash
git clone git@github.com:smaccoun/create-haskstar-app.git
cd create-haskstar-app
./install.sh
```

**NOTE** *The first build may take a VERY long time, especially if you do not have lts-9.4*
**After the first time builds should be quite fast! Grab a coffee during the initial setup! **



### Deployment Setup

Additionally, if you want to setup remote deployments, you will need several libraries.
Currently, deployments are orchestrated by Kubernetes. Kubernetes allows for highly declarative cloud deployments, and will soon have very good native support from AWS, Google Cloud (GCE), and Digital Ocean.
Since Google Cloud currently has the best support, current implementation requires some gcloud specific setup. 

Here's a list of all tools needed for deployment:

- [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/) 
- [helm](https://docs.helm.sh/using_helm/#installing-helm)
- [gcloud](https://cloud.google.com/sdk/downloads) 

You will also already need a google cloud account setup with project. Once you have this, make sure to run:

```bash
gcloud config set project [PROJECT_ID]
gcloud config set compute/zone us-central1-b

export PROJECT_ID="$(gcloud config get-value project -q)"
```


# Development Philosophy

Major pluggable parts (such as Elm or Miso) will each be kept in a separate repo
that haskstar will build from (e.g. see [haskstar-elm](https://github.com/smaccoun/haskstar-elm)). 
That way these can be used also be used as standalone templates

I am working on this full time and will take a very iterative approach. 
Idea is to get stack working for a certain set of configurations, then continually abstract until nearly anything is plug-in-able



# Roadmap

### Front-End

#### Frameworks
|   lib    | Status   |
|----------|:---------|
| Elm      |   ✅ 
| [Miso](https://github.com/dmjio/miso)  | Coming soon |
| Reflex? (haskell)    |          |
| Halogen (purescript)    |          |


#### Auth Views

|   Feature | Status   |
|----------|:---------|
| Login/Register view |     ✅      |
| OAuth     |      |
| Cookie    |      |

#### Styling

|   Feature | Status   |
|----------|:---------|
| Bulma |     ✅      |
| Font Awesome |      |
| Ionic |      |

#### Admin Panel

|   Feature | Status   |
|----------|:---------|
| Elm |     ✅      |
| Miso | Coming soon...target as isolated      |


### Back-End

#### Libraries
| lib                                                    | Status      |
|--------------------------------------------------------|-------------|
| [Servant](https://hackage.haskell.org/package/servant) | ✅          |
| [Beam](https://tathougies.github.io/beam/)             | ✅          |
| [Selda](https://selda.link/)   |  ?? 
| yesod?                                                 |             |

#### Auth

|   Feature | Status   |
|----------|:---------|
| password hasing (scrypt) |   ✅    |
| OAuth     |      |
| Cookie    |      |

#### Features
| feature                          | Status      |
|--------------------------------------------------------|-------------|
| Automatic Swagger UI generation  | ✅          |
| Automatic Swagger UI server      | ✅          |

### Ops

|   Platforms | Status   |
|----------|:---------|
| CircleCI |    ✅      |
| AWS      |       |
| GCloud   |    ✅   |

### DB

#### Migrations

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



