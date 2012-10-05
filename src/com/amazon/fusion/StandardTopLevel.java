// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
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
                     String initialModulePath)
        throws FusionException
    {
        mySystem    = system;
        myEvaluator = new Evaluator(mySystem, registry);
        myNamespace =
            myEvaluator.newNamespaceWithLanguage(registry, initialModulePath);
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
            result = myEvaluator.prepareAndEvalTopLevelForm(sourceExpr, myNamespace);
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
    public void define(String name, Object value)
    {
        myNamespace.bind(name, value);
    }
}
