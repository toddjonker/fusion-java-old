// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
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


    IonSexp readModuleDeclaration(Evaluator eval, String path)
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

                // TODO error handling
                IonSexp moduleDeclaration = (IonSexp) i.next();
                // TODO form must start with 'module'

                if (i.hasNext())
                {
                    String message =
                        "Module file has more than one top-level form: " +
                            path;
                    throw new FusionException(message);
                }

                return moduleDeclaration;
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

    ModuleInstance loadModule(Evaluator eval, String path)
        throws FusionException
    {
        IonValue moduleDeclaration = readModuleDeclaration(eval, path);

        // TODO don't create a copy of the kernel
        // TODO Do we need an Evaluator with no continuation marks?
        Namespace namespace = new CoreEnvironment(eval);
        FusionValue result = eval.eval(namespace, moduleDeclaration);
        // TODO tail call handling
        return (ModuleInstance) result;
    }
}
