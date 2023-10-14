#See https://aka.ms/customizecontainer to learn how to customize your debug container and how Visual Studio uses this Dockerfile to build your images for faster debugging.

FROM mcr.microsoft.com/dotnet/aspnet:7.0 AS base
WORKDIR /app
EXPOSE 80
EXPOSE 443

FROM mcr.microsoft.com/dotnet/sdk:7.0 AS build
WORKDIR /src
COPY ["Directory.build.props", "."]
COPY ["examples/TodoLists/Examples.TodoLists.WebApi/Examples.TodoLists.WebApi.fsproj", "examples/TodoLists/Examples.TodoLists.WebApi/"]
RUN dotnet restore "examples/TodoLists/Examples.TodoLists.WebApi/Examples.TodoLists.WebApi.fsproj"
COPY . .
WORKDIR "/src/examples/TodoLists/Examples.TodoLists.WebApi"
RUN dotnet build "Examples.TodoLists.WebApi.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "Examples.TodoLists.WebApi.fsproj" -c Release -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Examples.TodoLists.WebApi.dll"]