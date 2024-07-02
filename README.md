# State Monad For The Rest of Us

## Creation
```bash
mkdir state-monad-for-the-rest-of-us
cd state-monad-for-the-rest-of-us
git init

dotnet new sln

mkdir src
cd src
dotnet new xunit -lang F# -o StateMonadTest

cd ..
dotnet sln add src/StateMonadTest/StateMonadTest.fsproj
```
