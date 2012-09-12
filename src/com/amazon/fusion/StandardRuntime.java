// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import com.amazon.ion.system.IonSystemBuilder;
import java.io.File;

/**
 *
 */
final class StandardRuntime
    implements FusionRuntime
{
    private final IonSystem mySystem;
    private final Evaluator myEvaluator;
    private final Namespace myNamespace;


    /**
     * WARNING: if the IonSystem is ever configurable, be sure to verify that
     * {@link FusionValue#ionValue(com.amazon.ion.ValueFactory)} works right!
     *
     * @see {@link FusionValueTest#testIonValue()}.
     */
    StandardRuntime(FusionRuntimeBuilder builder)
    {
        mySystem = IonSystemBuilder.standard().build();

        final ModuleRegistry defaultRegistry = new ModuleRegistry();
        try
        {
            ModuleNamespace ns = new ModuleNamespace(defaultRegistry,
                                                     KernelModule.IDENTITY);
            ModuleInstance kernel = new KernelModule(mySystem, builder, ns);
            defaultRegistry.register(kernel);
        }
        catch (FusionException e)
        {
            throw new RuntimeException("Should not happen", e);
        }

        myEvaluator = new Evaluator(mySystem, defaultRegistry);
        try
        {
            myNamespace = myEvaluator.newBaseNamespace();
        }
        catch (FusionException e)
        {
            throw new RuntimeException("This shouldn't happen!", e);
        }
    }


    //========================================================================


    @Override
    public FusionValue eval(String source)
        throws ExitException, FusionException
    {
        IonReader i = mySystem.newReader(source);
        return eval(i);
    }


    @Override
    public FusionValue eval(IonReader source)
        throws ExitException, FusionException
    {
        FusionValue result = UNDEF;

        // TODO should work even if already positioned on first value

        while (source.next() != null)
        {
            SyntaxValue sourceExpr = Syntax.read(source);
            result = myEvaluator.prepareAndEvalTopLevelForm(sourceExpr, myNamespace);
        }

        return result;
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
    public void bind(String name, FusionValue value)
    {
        myNamespace.bind(name, value);
    }
}
