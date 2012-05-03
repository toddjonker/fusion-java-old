// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Iterator;

/**
 *
 */
class LoadHandler
{
    private final DynamicParameter myCurrentDirectoryParameter;

    LoadHandler(DynamicParameter currentDirectoryParameter)
    {
        myCurrentDirectoryParameter = currentDirectoryParameter;
    }


    File resolvePath(Evaluator eval, String path)
        throws FusionException
    {
        // TODO error handling
        FusionValue cd = eval.applyNonTail(myCurrentDirectoryParameter);
        String cdPath = ((IonString) ((DomValue) cd).getDom()).stringValue();
        File cdFile = new File(cdPath);
        return new File(cdFile, path);
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


    private IonValue readSingleTopLevelValue(Evaluator eval, String path)
        throws FusionException
    {
        File file = resolvePath(eval, path);
        try
        {
            FileInputStream in = new FileInputStream(file);
            try
            {
                Iterator<IonValue> i = eval.getSystem().iterate(in);
                if (! i.hasNext())
                {
                    String message =
                        "Module file has no top-level forms: " + path;
                   throw new FusionException(message);
                }

                IonValue firstTopLevel = i.next();
                if (i.hasNext())
                {
                    String message =
                        "Module file has more than one top-level form: " +
                        path;
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


    private IonSexp readModuleDeclaration(Evaluator eval, String path)
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


    ModuleInstance loadModule(Evaluator eval, String path)
        throws FusionException
    {
        IonValue moduleDeclaration = readModuleDeclaration(eval, path);

        // TODO Do we need an Evaluator with no continuation marks?
        // This namespace ensures correct binding for 'module'
        Namespace namespace = eval.newBaseNamespace();
        FusionValue result = eval.eval(namespace, moduleDeclaration);
        // TODO tail call handling
        return (ModuleInstance) result;
    }
}
