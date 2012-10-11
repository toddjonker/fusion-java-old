// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import static com.amazon.fusion.FusionValue.writeToString;
import static com.amazon.ion.util.IonTextUtils.printQuotedSymbol;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import java.io.File;


final class StandardTopLevel
    implements TopLevel
{
    private final IonSystem mySystem;
    private final Evaluator myEvaluator;
    private final Namespace myNamespace;


    StandardTopLevel(IonSystem system,
                     ModuleRegistry registry,
                     Namespace namespace,
                     String initialModulePath)
        throws FusionException
    {
        mySystem    = system;
        myEvaluator = new Evaluator(mySystem, registry);
        myNamespace = namespace;
        namespace.use(myEvaluator, initialModulePath);
    }

    StandardTopLevel(IonSystem system,
                     ModuleRegistry registry,
                     String initialModulePath)
        throws FusionException
    {
        this(system, registry, new Namespace(registry), initialModulePath);
    }


    //========================================================================


    @Override
    public Object eval(String source, SourceName name)
        throws ExitException, FusionException
    {
        IonReader i = mySystem.newReader(source);
        return eval(i, name);
    }


    @Override
    public Object eval(String source)
        throws ExitException, FusionException
    {
        return eval(source, null);
    }


    @Override
    public Object eval(IonReader source, SourceName name)
        throws ExitException, FusionException
    {
        Object result = UNDEF;

        // TODO should work even if already positioned on first value

        while (source.next() != null)
        {
            SyntaxValue sourceExpr = Syntax.read(source, name);
            result = myEvaluator.eval(sourceExpr, myNamespace);
        }

        return result;
    }


    @Override
    public Object eval(IonReader source)
        throws ExitException, FusionException
    {
        return eval(source, null);
    }


    @Override
    public Object load(File source)
        throws ExitException, FusionException
    {
        KernelModule kernel = myEvaluator.findKernel();
        LoadHandler load = kernel.getLoadHandler();
        return load.loadTopLevel(myEvaluator, myNamespace, source.toString());
    }


    @Override
    public void requireModule(String moduleIdentifier)
        throws FusionException
    {
        myNamespace.use(myEvaluator, moduleIdentifier);
    }


    @Override
    public void define(String name, Object value)
    {
        myNamespace.bind(name, value);
    }


    private Procedure lookupProcedure(String procedureName)
        throws FusionException
    {
        SyntaxSymbol id = SyntaxSymbol.make(procedureName);

        Object proc = myEvaluator.eval(id, myNamespace);
        if (proc instanceof Procedure)
        {
            return (Procedure) proc;
        }

        if (proc == null)
        {
            throw new FusionException(printQuotedSymbol(procedureName) +
                                      " is not defined");
        }

        throw new FusionException(printQuotedSymbol(procedureName) +
                                  " is not a procedure: " +
                                  writeToString(proc));
    }

    @Override
    public Object call(String procedureName, Object... arguments)
        throws FusionException
    {
        Procedure proc = lookupProcedure(procedureName);

        for (int i = 0; i < arguments.length; i++)
        {
            Object arg = arguments[i];
            arg = myEvaluator.injectMaybe(arg);
            if (arg == null)
            {
                throw new ArgTypeFailure("TopLevel.injectAndCall",
                                         "injectable Java type",
                                         i, arguments[i]);
            }
            arguments[i] = arg;
        }

        return myEvaluator.callNonTail(proc, arguments);
    }
}
