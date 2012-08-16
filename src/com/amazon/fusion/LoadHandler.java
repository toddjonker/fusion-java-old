// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Parallel to Racket's load handler.
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


    /**
     * Resolve a relative path against the {@code current_directory} param.
     * If file is absolute it is returned as-is.
     */
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

    /**
     * Resolve a relative path against the {@code current_directory} param.
     * If the path is absolute it is returned as-is.
     */
    private File resolvePath(Evaluator eval, String path)
        throws FusionException
    {
        File file = new File(path);
        return resolvePath(eval, file);
    }


    /**
     * Reads top-level syntax forms from a file, evaluating each in sequence.
     *
     * @param eval
     * @param namespace
     * @param path the file to read; may be relative, in which case it is
     * resolved relative to the {@code current_directory} parameter.

     * @return the value of the last form in the file, or null if the file
     * contains no forms.
     */
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

                IonReader reader = eval.getSystem().newReader(in);
                while (reader.next() != null)
                {
                    result = null;  // Don't hold onto garbage
                    SyntaxValue fileExpr = Syntax.read(reader);
                    result = eval.prepareAndEval(namespace, fileExpr);
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


    private SyntaxValue readSingleTopLevelValue(Evaluator eval,
                                                ModuleIdentity id)
        throws FusionException
    {
        try
        {
            InputStream in = id.open();
            try
            {
                IonReader reader = eval.getSystem().newReader(in);
                if (reader.next() == null)
                {
                    String message =
                        "Module file has no top-level forms: " + id;
                   throw new FusionException(message);
                }

                SyntaxValue firstTopLevel = Syntax.read(reader);
                if (reader.next() != null)
                {
                    String message =
                        "Module file has more than one top-level form: " +
                        id;
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


    private SyntaxSexp readModuleDeclaration(Evaluator eval, ModuleIdentity id)
        throws FusionException
    {
        SyntaxValue topLevel = readSingleTopLevelValue(eval, id);
        try {
            SyntaxSexp moduleDeclaration = (SyntaxSexp) topLevel;
            if (moduleDeclaration.size() > 1)
            {
                SyntaxSymbol moduleSym = (SyntaxSymbol)
                    moduleDeclaration.get(0);
                if ("module".equals(moduleSym.stringValue()))
                {
                    return moduleDeclaration;
                }
            }
        }
        catch (ClassCastException e) { /* fall through */ }

        String message = "Top-level form isn't (module ...): " + id;
        throw new FusionException(message);
    }


    /**
     *
     */
    ModuleInstance loadModule(Evaluator eval, ModuleIdentity id)
        throws FusionException
    {
        try
        {
            SyntaxValue moduleDeclaration = readModuleDeclaration(eval, id);

            Evaluator bodyEval = eval;
            String dirPath = id.parentDirectory();
            if (dirPath != null)
            {
                bodyEval =
                    eval.markedContinuation(myCurrentLoadRelativeDirectory,
                                            eval.newString(dirPath));
            }

            // TODO Do we need an Evaluator with no continuation marks?
            // This namespace ensures correct binding for 'module'

            ModuleRegistry reg = eval.getModuleRegistry();
            Namespace namespace = eval.newModuleNamespace(reg);
            FusionValue result =
                bodyEval.prepareAndEval(namespace, moduleDeclaration);
            // TODO tail call handling
            return (ModuleInstance) result;
        }
        catch (FusionException e)
        {
            String message =
                "Failure loading module from " + id.identify() +
                ": " + e.getMessage();
            throw new FusionException(message, e);
        }
    }
}
