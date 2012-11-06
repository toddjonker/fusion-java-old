// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import static com.amazon.fusion.ModuleIdentity.intern;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonType;

/**
 * The core set of objects from that are needed by other parts of the
 * implementation.
 */
final class GlobalState
{
    static final String KERNEL_MODULE_NAME = "#%kernel";
    static final ModuleIdentity KERNEL_MODULE_IDENTITY =
        intern(KERNEL_MODULE_NAME);

    final IonSystem          myIonSystem;
    final ModuleInstance     myKernelModule;
    final ModuleNameResolver myModuleNameResolver;
    final LoadHandler        myLoadHandler;
    final UseForm            myUseForm;
    final DynamicParameter   myCurrentNamespaceParam;


    private GlobalState(IonSystem          ionSystem,
                        ModuleInstance     kernel,
                        ModuleNameResolver resolver,
                        LoadHandler        loadHandler,
                        UseForm            useForm,
                        DynamicParameter   currentNamespaceParam)
    {
        myIonSystem             = ionSystem;
        myKernelModule          = kernel;
        myModuleNameResolver    = resolver;
        myLoadHandler           = loadHandler;
        myUseForm               = useForm;
        myCurrentNamespaceParam = currentNamespaceParam;
    }


    static GlobalState initialize(IonSystem system,
                                  FusionRuntimeBuilder builder,
                                  ModuleRegistry registry,
                                  Namespace initialCurrentNamespace)
        throws FusionException
    {
        ModuleBuilderImpl ns =
            new ModuleBuilderImpl(registry, KERNEL_MODULE_IDENTITY);

        Object userDir =
            FusionValue.forIonValue(system.newString(builder.getInitialCurrentDirectory().toString()));
        DynamicParameter currentDirectory =
            new DynamicParameter(userDir);
        DynamicParameter currentLoadRelativeDirectory =
            new DynamicParameter(UNDEF);
        DynamicParameter currentModuleDeclareName =
            new DynamicParameter(UNDEF);
        DynamicParameter currentNamespaceParam =
            new DynamicParameter(initialCurrentNamespace);
        LoadHandler loadHandler =
            new LoadHandler(currentLoadRelativeDirectory, currentDirectory);
        ModuleNameResolver resolver =
            new ModuleNameResolver(loadHandler,
                                   currentLoadRelativeDirectory,
                                   currentDirectory,
                                   currentModuleDeclareName,
                                   builder.buildModuleRepositories());
        UseForm useForm = new UseForm(resolver);

        // These must be bound before 'module' since we need the bindings
        // for the partial-expansion stop-list.
        ns.define("define", new DefineForm());
        ns.define("define_syntax", new DefineSyntaxForm());
        ns.define("use", useForm);

        SyntacticForm moduleForm =
            new ModuleForm(resolver, currentModuleDeclareName);
        LoadProc loadProc = new LoadProc(loadHandler);

        ns.define("begin", new BeginForm());    // Needed by hard-coded macro
        ns.define("current_directory", currentDirectory);
        ns.define("current_namespace", currentNamespaceParam);
        ns.define("empty_stream", FusionValue.EMPTY_STREAM);
        ns.define("if", new IfForm());          // Needed by hard-coded macro
        ns.define("java_new", new JavaNewProc());
        ns.define("lambda", new LambdaForm());  // Needed by hard-coded macro
        ns.define("letrec", new LetrecForm());  // Needed by hard-coded macro
        ns.define("load", loadProc);
        ns.define("module", moduleForm);
        ns.define("quote_syntax", new QuoteSyntaxForm()); // For fusion/syntax
        ns.define("undef", FusionValue.UNDEF);

        for (IonType t : IonType.values())
        {
            if (t != IonType.NULL &&
                t != IonType.DATAGRAM &&
                t != IonType.LIST)
            {
                String name = "is_" + t.name().toLowerCase();
                ns.define(name, new IonTypeCheckingProc(t));
            }
        }

        ns.define("is_list", new IsListProc());

        ModuleInstance kernel = ns.build();
        registry.register(kernel);

        GlobalState globals =
            new GlobalState(system, kernel, resolver, loadHandler, useForm,
                            currentNamespaceParam);
        return globals;
    }
}
