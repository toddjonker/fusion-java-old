// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleIdentity.intern;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonType;


/**
 * The kernel bindings for Fusion, the bare minimum that needs to be done
 * using Java code.  This module isn't for end users.
 */
final class KernelModule
    extends ModuleInstance
{
    static final String NAME = "#%kernel";
    static final ModuleIdentity IDENTITY = intern(NAME);


    private final GlobalState myGlobalState;


    KernelModule(IonSystem system, FusionRuntimeBuilder builder,
                 ModuleNamespace ns, Namespace currentNamespace)
        throws FusionException
    {
        super(IDENTITY, ns);
        inferName(NAME);

        Object userDir =
            FusionValue.forIonValue(system.newString(builder.getInitialCurrentDirectory().toString()));
        DynamicParameter currentDirectory =
            new DynamicParameter(userDir);
        DynamicParameter currentLoadRelativeDirectory =
            new DynamicParameter(UNDEF);
        DynamicParameter currentModuleDeclareName =
            new DynamicParameter(UNDEF);
        DynamicParameter currentNamespaceParam =
            new DynamicParameter(currentNamespace);
        LoadHandler loadHandler =
            new LoadHandler(currentLoadRelativeDirectory, currentDirectory);
        ModuleNameResolver resolver =
            new ModuleNameResolver(loadHandler,
                                   currentLoadRelativeDirectory,
                                   currentDirectory,
                                   currentModuleDeclareName,
                                   builder.buildModuleRepositories());
        UseKeyword useKeyword = new UseKeyword(resolver);

        // These must be bound before 'module' since we need the bindings
        // for the partial-expansion stop-list.
        ns.bind("define", new DefineKeyword());
        ns.bind("define_syntax", new DefineSyntaxKeyword());
        ns.bind("use", useKeyword);

        KeywordValue moduleKeyword =
            new ModuleKeyword(resolver, currentModuleDeclareName, ns);
        LoadProc loadProc = new LoadProc(loadHandler);

        ns.bind("begin", new BeginKeyword());    // Needed by hard-coded macro
        ns.bind("current_directory", currentDirectory);
        ns.bind("current_namespace", currentNamespaceParam);
        ns.bind("empty_stream", FusionValue.EMPTY_STREAM);
        ns.bind("if", new IfKeyword());          // Needed by hard-coded macro
        ns.bind("java_new", new JavaNewProc());
        ns.bind("lambda", new LambdaKeyword());  // Needed by hard-coded macro
        ns.bind("letrec", new LetrecKeyword());  // Needed by hard-coded macro
        ns.bind("load", loadProc);
        ns.bind("module", moduleKeyword);
        ns.bind("quote_syntax", new QuoteSyntaxKeyword()); // For fusion/syntax
        ns.bind("undef", FusionValue.UNDEF);

        for (IonType t : IonType.values())
        {
            if (t != IonType.NULL &&
                t != IonType.DATAGRAM &&
                t != IonType.LIST)
            {
                String name = "is_" + t.name().toLowerCase();
                ns.bind(name, new IonTypeCheckingProc(t));
            }
        }

        ns.bind("is_list", new IsListProc());

        provideEverything();

        myGlobalState = new GlobalState(system, this, loadHandler, useKeyword, currentNamespaceParam);
    }

    GlobalState getGlobalState()
    {
        return myGlobalState;
    }
}
