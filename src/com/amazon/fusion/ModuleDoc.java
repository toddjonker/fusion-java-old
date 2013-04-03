// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import java.io.File;
import java.io.IOException;
import java.text.BreakIterator;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;


final class ModuleDoc
{
    private final FusionRuntime myRuntime;
    private final String myName;
    final String myPath;
    final String myIntroDocs;

    private Map<String,ModuleDoc>  mySubmodules;
    private Map<String,BindingDoc> myBindings;


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


    public static ModuleDoc buildDocTree(FusionRuntime runtime, File repoDir)
        throws IOException, FusionException
    {
        ModuleDoc doc = new ModuleDoc(runtime, null, "");
        buildTree(repoDir, doc);
        return doc;
    }


    private static void buildTree(File dir, ModuleDoc doc)
        throws IOException, FusionException
    {
        String[] fileNames = dir.list();

        // First pass: build all "real" modules
        for (String fileName : fileNames)
        {
            if (fileName.endsWith(".ion"))
            {
                // We assume that all .ion files are modules.
                String moduleName =
                    fileName.substring(0, fileName.length() - 4);
                doc.addSubmodule(moduleName);
            }
        }

        // Second pass: look for directories, which are implicitly submodules.
        for (String fileName : fileNames)
        {
            if (fileName.equals("private")) continue;

            File testFile = new File(dir, fileName);
            if (testFile.isDirectory())
            {
                ModuleDoc d = doc.implicitSubmodule(fileName);
                buildTree(testFile, d);
            }
        }
    }


    /**
     * @param name can be null to represent the repository root (not really a
     *  module).
     */
    private ModuleDoc(FusionRuntime runtime, String name, String path)
        throws FusionException
    {
        myRuntime = runtime;
        myName = name;
        myPath = path;

        String docs = null;
        if (name != null)
        {
            StandardRuntime rt = (StandardRuntime) runtime;
            ModuleRegistry registry = rt.getDefaultRegistry();
            try
            {
                ModuleIdentity id = resolveModulePath(runtime, path);
                ModuleInstance moduleInstance = registry.lookup(id);

                docs = moduleInstance.getDocs();

                build(moduleInstance);
            }
            catch (ModuleNotFoundFailure e) { }
        }
        myIntroDocs = docs;
    }

    private ModuleDoc(ModuleDoc parent, String name)
        throws FusionException
    {
        this(parent.myRuntime, name, parent.myPath + "/" + name);
    }


    String baseName()
    {
        return myName;
    }

    String oneLiner()
    {
        if (myIntroDocs == null) return null;

        // TODO pick a better locale?
        BreakIterator breaks = BreakIterator.getSentenceInstance();
        breaks.setText(myIntroDocs);
        int start = breaks.first();
        int end = breaks.next();
        if (end == BreakIterator.DONE) return null;

        return myIntroDocs.substring(start, end);
    }


    Map<String, BindingDoc> bindingMap()
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

    Map<String, ModuleDoc> submoduleMap()
    {
        return mySubmodules;
    }

    Collection<ModuleDoc> submodules()
    {
        if (mySubmodules == null)
        {
            return Collections.emptySet();
        }
        return mySubmodules.values();
    }


    private void build(ModuleInstance module)
    {
        Set<String> names = module.providedNames();
        if (names.size() == 0) return;

        myBindings = new HashMap<String,BindingDoc>(names.size());

        for (String name : names)
        {
            BindingDoc doc = module.documentProvidedName(name);
            myBindings.put(name, doc);
        }
    }


    private ModuleDoc addSubmodule(String name)
        throws FusionException
    {
        ModuleDoc doc = new ModuleDoc(this, name);

        if (mySubmodules == null)
        {
            mySubmodules = new HashMap<String,ModuleDoc>();
        }

        assert ! mySubmodules.containsKey(name);
        mySubmodules.put(name, doc);

        return doc;
    }


    private ModuleDoc implicitSubmodule(String name)
        throws FusionException
    {
        if (mySubmodules != null)
        {
            ModuleDoc doc = mySubmodules.get(name);

            if (doc != null) return doc;
        }

        return addSubmodule(name);
    }
}
