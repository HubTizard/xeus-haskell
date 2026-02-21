const installLegacyWasmFsMountShim = () => {
  if (!Module?.FS || typeof Module.FS.mount !== "function") {
    return;
  }
  if (Module.FS.__xhLegacyWasmFsMountShimInstalled) {
    return;
  }

  const originalMount = Module.FS.mount.bind(Module.FS);

  Module.FS.mount = (type, opts, mountpoint) => {
    if (!type || typeof type.createBackend === "function") {
      return originalMount(type, opts, mountpoint);
    }

    // jupyterlite-xeus can still pass legacy FS mount objects.
    if (typeof _wasmfs_create_memory_backend !== "function") {
      console.warn("[xeus-haskell] WasmFS shim unavailable, using original mount");
      return originalMount(type, opts, mountpoint);
    }

    const typeName = type.name || type.constructor?.name || "legacy-fs-type";
    console.warn(
      `[xeus-haskell] WasmFS shim: ${typeName} -> memory backend at ${mountpoint}`
    );
    return originalMount(
      { createBackend: () => _wasmfs_create_memory_backend() },
      opts,
      mountpoint
    );
  };

  Module.FS.__xhLegacyWasmFsMountShimInstalled = true;
};

const previousPreRun = Module.preRun;
Module.preRun = () => {
  if (Array.isArray(previousPreRun)) {
    previousPreRun.forEach((fn) => typeof fn === "function" && fn());
  } else if (typeof previousPreRun === "function") {
    previousPreRun();
  }

  ENV.MHSDIR = "/share/microhs";
  ENV.MHS_LIBRARY_PATH = "/usr/lib/haskell-packages/microhs";
  installLegacyWasmFsMountShim();
};

const previousRuntimeInitialized = Module.onRuntimeInitialized;
Module.onRuntimeInitialized = () => {
  installLegacyWasmFsMountShim();
  if (typeof previousRuntimeInitialized === "function") {
    previousRuntimeInitialized();
  }
};
