# State Monad For The Rest of Us

Source code for the series of articles [State Monad For The Rest of Us](https://arialdomartini.github.io/state-monad-for-the-rest-of-us).

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
