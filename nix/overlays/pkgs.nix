# our packages overlay
self: super: {
  # Our jira installation needs API tokens to use Bearer authentication, not Basic.
  go-jira = super.go-jira.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or []) ++ [ ./go-jira-api-token.patch ];
  });

}
