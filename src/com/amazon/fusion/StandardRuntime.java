// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleIdentity.BUILTIN_NAME_EXPECTATION;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;
import com.amazon.ion.system.IonSystemBuilder;


final class StandardRuntime
    implements FusionRuntime
{
    private final GlobalState      myGlobalState;
    private final ModuleRegistry   myRegistry;
    private final StandardTopLevel myTopLevel;


    StandardRuntime(FusionRuntimeBuilder builder)
    {
        IonSystem ionSystem = IonSystemBuilder.standard().build();
        myRegistry = new ModuleRegistry();

        try
        {
            // This is the bootstrap top-level namespace, which starts out
            // empty.  It becomes the initial value of current_namespace
            // during global initialization.
            Namespace topNs = new TopLevelNamespace(myRegistry);

            myGlobalState =
                GlobalState.initialize(ionSystem, builder, myRegistry, topNs);

            myTopLevel =
                new StandardTopLevel(myGlobalState, topNs,
                                     getDefaultLanguage(),
                                     builder.isDocumenting());
        }
        catch (FusionException e)
        {
            throw new RuntimeException("Should not happen", e);
        }
    }


    // Not public
    GlobalState getGlobalState()
    {
        return myGlobalState;
    }

    // Not public
    ModuleRegistry getDefaultRegistry()
    {
        return myRegistry;
    }

    String getDefaultLanguage()
    {
        return "/fusion/base";
    }


    //========================================================================


    @Override
    public StandardTopLevel getDefaultTopLevel()
        throws FusionException
    {
        return myTopLevel;
    }


    @Override
    public TopLevel makeTopLevel(String initialModulePath)
        throws FusionException
    {
        return new StandardTopLevel(myGlobalState, myRegistry,
                                    initialModulePath);
    }


    @Override
    public TopLevel makeTopLevel()
        throws FusionException
    {
        return makeTopLevel(getDefaultLanguage());
    }


    @Override
    public ModuleBuilder makeModuleBuilder(String moduleName)
    {
        if (! ModuleIdentity.isValidBuiltinName(moduleName))
        {
            String message =
                "Invalid built-in module name. " + BUILTIN_NAME_EXPECTATION;
            throw new IllegalArgumentException(message);
        }

        ModuleIdentity id = ModuleIdentity.internBuiltinName(moduleName);
        return new ModuleBuilderImpl(myRegistry, id);
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
