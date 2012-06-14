// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Iterator;

/**
 *
 */
final class LoadHandler
{
    private final DynamicParameter myCurrentLoadRelativeDirectory;
    private final DynamicParameter myCurrentDirectory;

    LoadHandler(DynamicParameter currentLoadRelativeDirectory,
                DynamicParameter currentDirectory)
    {
        myCurrentLoadRelativeDirectory = currentLoadRelativeDirectory;
        myCurrentDirectory = currentDirectory;
    }


    private File resolvePath(Evaluator eval, File file)
        throws FusionException
    {
        if (! file.isAbsolute())
        {
            String cdPath = myCurrentDirectory.asString(eval);
            File cdFile = new File(cdPath);
            file = new File(cdFile, file.getPath());
        }
        return file;
    }

    private File resolvePath(Evaluator eval, String path)
        throws FusionException
    {
        File file = new File(path);
        return resolvePath(eval, file);
    }


    FusionValue loadTopLevel(Evaluator eval, Namespace namespace, String path)
        throws FusionException
    {
        File file = resolvePath(eval, path);
        try
        {
            FileInputStream in = new FileInputStream(file);
            try
            {
                FusionValue result = null;

                Iterator<IonValue> i = eval.getSystem().iterate(in);
                while (i.hasNext())
                {
                    result = null;  // Don't hold onto garbage
                    IonValue fileExpr = i.next();
                    result = eval.eval(namespace, fileExpr);
                    // TODO tail call handling
                }

                return result;
            }
            finally
            {
                in.close();
            }
        }
        catch (IOException e)
        {
            throw new FusionException(e);
        }
    }


    private IonValue readSingleTopLevelValue(Evaluator eval, File file)
        throws FusionException
    {
        try
        {
            FileInputStream in = new FileInputStream(file);
            try
            {
                Iterator<IonValue> i = eval.getSystem().iterate(in);
                if (! i.hasNext())
                {
                    String message =
                        "Module file has no top-level forms: " + file;
                   throw new FusionException(message);
                }

                IonValue firstTopLevel = i.next();
                if (i.hasNext())
                {
                    String message =
                        "Module file has more than one top-level form: " +
                        file;
                    throw new FusionException(message);
                }

                return firstTopLevel;
            }
            finally
            {
                in.close();
            }
        }
        catch (IOException e)
        {
            throw new FusionException(e);
        }
    }


    private IonSexp readModuleDeclaration(Evaluator eval, File path)
        throws FusionException
    {
        IonValue topLevel = readSingleTopLevelValue(eval, path);
        try {
            IonSexp moduleDeclaration = (IonSexp) topLevel;
            if (moduleDeclaration.size() > 1)
            {
                IonSymbol moduleSym = (IonSymbol) moduleDeclaration.get(0);
                if ("module".equals(moduleSym.stringValue()))
                {
                    return moduleDeclaration;
                }
            }
        }
        catch (ClassCastException e) { /* fall through */ }

        String message = "Top-level form isn't (module ...): " + path;
        throw new FusionException(message);
    }


    ModuleInstance loadModule(Evaluator eval, File path)
        throws FusionException
    {
        File file = resolvePath(eval, path);
        IonValue moduleDeclaration = readModuleDeclaration(eval, file);

        String dirPath = file.getParentFile().getAbsolutePath();
        Evaluator bodyEval =
            eval.markedContinuation(myCurrentLoadRelativeDirectory,
                                    eval.newString(dirPath));

        // TODO Do we need an Evaluator with no continuation marks?
        // This namespace ensures correct binding for 'module'

        ModuleRegistry reg = eval.getModuleRegistry();
        Namespace namespace = eval.newModuleNamespace(reg);
        FusionValue result = bodyEval.eval(namespace, moduleDeclaration);
        // TODO tail call handling
        return (ModuleInstance) result;
    }
}
