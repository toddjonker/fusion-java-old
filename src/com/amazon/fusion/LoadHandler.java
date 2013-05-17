// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionEval.callCurrentEval;
import static com.amazon.fusion.GlobalState.MODULE;
import com.amazon.ion.IonException;
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
     * The file is resolved relative to {@code current_directory}.
     * Loading is parameterized to set {@code current_load_relative_directory}
     * to the parent of the resolved path.
     *
     * @param eval
     * @param namespace may be null, to use
     *   {@link Evaluator#findCurrentNamespace()}.
     * @param path the file to read; may be relative, in which case it is
     * resolved relative to the {@code current_directory} parameter.

     * @return the value of the last form in the file, or null if the file
     * contains no forms.
     */
    Object loadTopLevel(Evaluator eval, Namespace namespace, String path)
        throws FusionException
    {
        File file = resolvePath(eval, path);
        File parent = file.getParentFile();

        eval = eval.markedContinuation(myCurrentLoadRelativeDirectory,
                                       eval.newString(parent.getAbsolutePath()));

        try
        {
            FileInputStream in = new FileInputStream(file);
            try
            {
                SourceName name = SourceName.forFile(file);
                Object result = null;

                IonReader reader = eval.getSystem().newReader(in);
                while (reader.next() != null)
                {
                    result = null;  // Don't hold onto garbage
                    SyntaxValue fileExpr = Syntax.read(eval, reader, name);
                    result = FusionEval.eval(eval, fileExpr, namespace);
                    // TODO TAIL
                }

                return result;
            }
            finally
            {
                in.close();
            }
        }
        catch (IOException | IonException e)
        {
            String message =
                "Error loading file " + file + ": " + e.getMessage();
            throw new FusionException(message, e);
        }
    }


    private SyntaxValue readModuleSyntax(Evaluator eval, ModuleIdentity id)
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

                SourceName name = SourceName.forModule(id);
                SyntaxValue firstTopLevel = Syntax.read(eval, reader, name);
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
        SyntaxValue topLevel = readModuleSyntax(eval, id);
        try
        {
            SyntaxSexp moduleDeclaration = (SyntaxSexp) topLevel;
            if (moduleDeclaration.size() > 1)
            {
                SyntaxSymbol moduleSym = (SyntaxSymbol)
                    moduleDeclaration.get(0);
                if (MODULE.equals(moduleSym.stringValue()))
                {
                    return moduleDeclaration;
                }
            }
        }
        catch (ClassCastException e) { /* fall through */ }

        String message = "Top-level form isn't (module ...)";
        throw new SyntaxFailure("load handler", message, topLevel);
    }

    private SyntaxSexp
    wrapModuleIdentifierWithKernelBindings(Evaluator eval,
                                           SyntaxSexp moduleStx)
    {
        SyntaxValue[] children = moduleStx.extract();

        // We already verified this type-safety
        assert ((SyntaxSymbol) children[0]).stringValue().equals(MODULE);

        children[0] = eval.getGlobalState().myKernelModuleIdentifier;

        moduleStx = SyntaxSexp.make(eval, moduleStx.getLocation(), children);
        return moduleStx;
    }

    /**
     *
     */
    void loadModule(Evaluator eval, ModuleIdentity id)
        throws FusionException
    {
        try
        {
            SyntaxSexp moduleDeclaration = readModuleDeclaration(eval, id);

            moduleDeclaration =
                wrapModuleIdentifierWithKernelBindings(eval, moduleDeclaration);

            Evaluator bodyEval = eval;
            String dirPath = id.parentDirectory();
            if (dirPath != null)
            {
                bodyEval =
                    eval.markedContinuation(myCurrentLoadRelativeDirectory,
                                            eval.newString(dirPath));
            }

            // TODO FUSION-40 separate module declaration from instantiation
            // This should result in a declaration, no instantiation or visit.

            // TODO Do we need an Evaluator with no continuation marks?

            callCurrentEval(bodyEval, moduleDeclaration);
            // TODO TAIL
        }
        catch (FusionException e)
        {
            String message =
                "Failure loading module " + id.identify() +
                ": " + e.getMessage();
            throw new FusionException(message, e);
        }
    }
}
