self: super: {
  basement = super.basement.overrideAttrs (oldAttrs: {
    CFLAGS = (oldAttrs.CFLAGS or "") + " -Wno-error=int-conversion";
  });
}
