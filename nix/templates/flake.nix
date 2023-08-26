{
  description = "templates to get subprojects started";

  outputs = { self }: {
    templates = {
      haskell-stack = {
        path = ./haskell-stack;
        description = "developer tools for a Haskell project with Stack";
      };
    };
  };
}
