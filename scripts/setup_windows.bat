REM This script must be run as an administrator
REM install chocolatey...

REM install nodejs
choco install nodejs.install -y
choco install yarn -y

REM install dependencies
yarn install
