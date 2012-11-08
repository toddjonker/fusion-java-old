// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import com.amazon.fusion.ModuleNamespace.ModuleBinding;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;


final class ModuleDocumentation
{
    private final FusionRuntime myRuntime;
    final String myName;
    final String myPath;

    private Map<String,ModuleDocumentation> mySubmodules;
    private Map<String,BindingDocumentation> myBindings;


    private static ModuleIdentity resolveModulePath(FusionRuntime runtime,
                                                    String modulePath)
        throws FusionException
    {
        assert modulePath.startsWith("/");

        StandardTopLevel top = (StandardTopLevel) runtime.getDefaultTopLevel();
        Evaluator eval = top.getEvaluator();
        ModuleNameResolver resolver =
            eval.getGlobalState().myModuleNameResolver;

        return resolver.resolveLib(eval, modulePath, null);
    }


    public static ModuleDocumentation buildDocTree(FusionRuntime runtime,
                                                   File repoDir)
        throws IOException, FusionException
    {
        ModuleDocumentation doc = new ModuleDocumentation(runtime, null, "");
        buildTree(repoDir, doc);
        return doc;
    }


    private static void buildTree(File dir, ModuleDocumentation doc)
        throws IOException, FusionException
    {
        String[] fileNames = dir.list();

        for (String fileName : fileNames)
        {
            if (fileName.equals("private")) continue;

            File testFile = new File(dir, fileName);
            if (testFile.isDirectory())
            {
                ModuleDocumentation d = doc.addSubmodule(fileName);
                buildTree(testFile, d);
            }
            else if (fileName.endsWith(".ion"))
            {
                // We assume that all .ion files are modules.
                String moduleName =
                    fileName.substring(0, fileName.length() - 4);
                ModuleDocumentation d = doc.addSubmodule(moduleName);
            }
        }
    }

    /**
     * @param name can be null to represent the repository root (not really a
     *  module).
     */
    private ModuleDocumentation(FusionRuntime runtime,
                                String name, String path)
        throws FusionException
    {
        myRuntime = runtime;
        myName = name;
        myPath = path;

        if (name != null)
        {
            StandardRuntime rt = (StandardRuntime) runtime;
            ModuleRegistry registry = rt.getDefaultRegistry();
            try
            {
                ModuleIdentity id = resolveModulePath(runtime, path);
                ModuleInstance moduleInstance = registry.lookup(id);

                build(moduleInstance, registry);
            }
            catch (ModuleNotFoundFailure e) { }
        }
    }

    private ModuleDocumentation(ModuleDocumentation parent, String name)
        throws FusionException
    {
        this(parent.myRuntime, name, parent.myPath + "/" + name);
    }


    Map<String, BindingDocumentation> bindingMap()
    {
        return myBindings;
    }

    String[] sortedExportedNames()
    {
        String[] names = EMPTY_STRING_ARRAY;
        if (myBindings != null)
        {
            names = myBindings.keySet().toArray(EMPTY_STRING_ARRAY);
            Arrays.sort(names);
        }
        return names;
    }

    Map<String, ModuleDocumentation> submoduleMap()
    {
        return mySubmodules;
    }

    Collection<ModuleDocumentation> submodules()
    {
        if (mySubmodules == null)
        {
            return Collections.emptySet();
        }
        return mySubmodules.values();
    }


    // TODO FUSION-83 shouldn't need registry
    private void build(ModuleInstance module, ModuleRegistry registry)
    {
        Set<String> names = module.providedNames();
        if (names.size() == 0) return;

        myBindings = new HashMap<String,BindingDocumentation>(names.size());

        for (String name : names)
        {
            ModuleBinding binding = module.resolveProvidedName(name);
            Object value = binding.lookup(module, registry);

            BindingDocumentation doc = null;
            if (value instanceof FusionValue)
            {
                FusionValue fv = (FusionValue) value;
                doc = fv.document();
                assert doc == null || name.equals(doc.myName);

                myBindings.put(name, doc);
            }
        }
    }


    private ModuleDocumentation addSubmodule(String name)
        throws FusionException
    {
        ModuleDocumentation doc = new ModuleDocumentation(this, name);

        if (mySubmodules == null)
        {
            mySubmodules = new HashMap<String,ModuleDocumentation>();
        }

        assert ! mySubmodules.containsKey(name);
        mySubmodules.put(name, doc);

        return doc;
    }
}
