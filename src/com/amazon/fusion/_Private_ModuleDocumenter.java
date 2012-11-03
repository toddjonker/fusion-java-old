// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import com.amazon.fusion.ModuleNamespace.ModuleBinding;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;

/**
 * NOT FOR APPLICATION USE
 */
public final class _Private_ModuleDocumenter
{
    private _Private_ModuleDocumenter() {}


    public static void documentModule(File out,
                                      FusionRuntime runtime,
                                      String modulePath)
        throws IOException, FusionException
    {
        out.getParentFile().mkdirs();

        FileWriter outWriter = new FileWriter(out);
        try
        {
            documentModule(outWriter, runtime, modulePath);
        }
        finally
        {
            outWriter.close();
        }
    }


    public static void documentModule(Appendable out,
                                      FusionRuntime runtime,
                                      String module)
        throws IOException, FusionException
    {
        StandardRuntime rt = (StandardRuntime) runtime;
        ModuleRegistry registry = rt.getDefaultRegistry();
        StandardTopLevel top = rt.getDefaultTopLevel();
        Evaluator eval = top.getEvaluator();
        documentModule(out, eval, registry, module);
    }


    // TODO FUSION-83 shouldn't need registry
    static void documentModule(Appendable out,
                               Evaluator eval,
                               ModuleRegistry registry,
                               String modulePath)
        throws IOException, FusionException
    {
        assert modulePath.startsWith("/");

        ModuleNameResolver resolver =
            eval.getGlobalState().myModuleNameResolver;
        ModuleIdentity id = resolver.resolveLib(eval, modulePath, null);
        ModuleInstance moduleInstance = registry.lookup(id);
        documentModule(eval, out, modulePath, moduleInstance, registry);
    }


    // TODO FUSION-83 shouldn't need registry
    private static void documentModule(Evaluator eval,
                                       Appendable out,
                                       String modulePath,
                                       ModuleInstance module,
                                       ModuleRegistry registry)
        throws IOException
    {
        assert modulePath.startsWith("/");

        displayHeader2(out, "Module " + modulePath);

        String[] names = module.providedNames().toArray(EMPTY_STRING_ARRAY);
        Arrays.sort(names);

        for (String name : names)
        {
            ModuleBinding binding = module.resolveProvidedName(name);
            Object value = binding.lookup(module, registry);

            FeatureDocumentation doc = null;
            if (value instanceof FusionValue)
            {
                FusionValue fv = (FusionValue) value;
                doc = fv.document();
                assert doc == null || name.equals(doc.myName);
            }

            documentFeature(out, name, doc);
        }
    }

    private static void documentFeature(Appendable out, String name,
                                        FeatureDocumentation doc)
        throws IOException
    {
        out.append("\n\n");
        displayHeader3(out, name);
        out.append('\n');

        if (doc == null)
        {
            out.append("No documentation available.\n");
        }
        else
        {
            out.append("    ");
            out.append(doc.myUsage);
            out.append('\n');

            out.append('*');
            // Using enum toString() allows display name to be changed
            out.append(doc.myKind.toString());

            if (doc.myBody == null)
            {
                out.append("*\n");
            }
            else
            {
                out.append(":* ");
                out.append('\n');
                out.append(doc.myBody);
                out.append('\n');
            }
        }
    }

    private static void displayHeader2(Appendable out, String text)
        throws IOException
    {
        out.append("## ");
        out.append(text);
        out.append('\n');
    }

    private static void displayHeader3(Appendable out, String text)
        throws IOException
    {
        out.append("### ");
        out.append(text);
        out.append('\n');
    }
}
