// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;
import com.amazon.ion.system.IonSystemBuilder;
import java.io.File;


final class StandardRuntime
    implements FusionRuntime
{
    private final IonSystem      mySystem;
    private final ModuleRegistry myRegistry;
    private final TopLevel       myTopLevel;


    StandardRuntime(FusionRuntimeBuilder builder)
    {
        mySystem   = IonSystemBuilder.standard().build();
        myRegistry = new ModuleRegistry();

        try
        {
            Namespace topNs = new Namespace(myRegistry);
            ModuleNamespace ns = new ModuleNamespace(myRegistry,
                                                     KernelModule.IDENTITY);
            ModuleInstance kernel = new KernelModule(mySystem, builder, ns,
                                                     topNs);
            myRegistry.register(kernel);

            myTopLevel = new StandardTopLevel(mySystem, myRegistry,
                                              topNs, "fusion/base");
        }
        catch (FusionException e)
        {
            throw new RuntimeException("Should not happen", e);
        }
    }


    //========================================================================


    @Override
    public TopLevel getDefaultTopLevel()
        throws FusionException
    {
        return myTopLevel;
    }


    @Override
    public TopLevel makeTopLevel(String initialModulePath)
        throws FusionException
    {
        return new StandardTopLevel(mySystem, myRegistry, initialModulePath);
    }


    @Override
    public TopLevel makeTopLevel()
        throws FusionException
    {
        return makeTopLevel("fusion/base");
    }


    //========================================================================


    @Override
    public Object eval(String source, SourceName name)
        throws ExitException, FusionException
    {
        return myTopLevel.eval(source, name);
    }


    @Override
    public Object eval(String source)
        throws ExitException, FusionException
    {
        return myTopLevel.eval(source);
    }


    @Override
    public Object eval(IonReader source, SourceName name)
        throws ExitException, FusionException
    {
        return myTopLevel.eval(source, name);
    }


    @Override
    public Object eval(IonReader source)
        throws ExitException, FusionException
    {
        return myTopLevel.eval(source);
    }


    @Override
    public Object load(File source)
        throws ExitException, FusionException
    {
        return myTopLevel.load(source);
    }


    @Override
    public void bind(String name, Object value)
    {
        myTopLevel.define(name, value);
    }

    //========================================================================


    @Override
    public IonValue ionize(Object fusionValue, ValueFactory factory)
        throws FusionException
    {
        return FusionValue.copyToIonValue(fusionValue, factory);
    }


    @Override
    public IonValue ionizeMaybe(Object fusionValue, ValueFactory factory)
        throws FusionException
    {
        return FusionValue.copyToIonValueMaybe(fusionValue, factory);
    }
}
