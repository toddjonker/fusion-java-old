// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BindingDoc.COLLECT_DOCS_MARK;
import static com.amazon.fusion.FusionEval.evalSyntax;
import static com.amazon.ion.util.IonTextUtils.printQuotedSymbol;
import static java.lang.Boolean.TRUE;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

/**
 * @see <a href="http://docs.racket-lang.org/reference/module.html">Racket
 * modules</a>
 */
final class ModuleForm
    extends SyntacticForm
{
    private final DynamicParameter myCurrentModuleDeclareName;
    private final ModuleNameResolver myModuleNameResolver;

    ModuleForm(ModuleNameResolver moduleNameResolver,
               DynamicParameter currentModuleDeclareName)
    {
        //    "                                                                               |
        super("name body ...+",
              "Declares a module containing the given body. The NAME must be a symbol.");

        myCurrentModuleDeclareName = currentModuleDeclareName;
        myModuleNameResolver = moduleNameResolver;
    }


    private Binding stopBinding(ModuleInstance kernel,
                                Map<Binding, Object> stops,
                                String name)
    {
        Binding b = kernel.resolveProvidedName(name).originalBinding();
        assert b != null;
        stops.put(b, TRUE);
        return b;
    }


    @Override
    SyntaxValue expand(Expander expander, Environment envOutsideModule,
                       SyntaxSexp source)
        throws FusionException
    {
        SyntaxChecker check = check(source);
        if (! expander.isTopLevelContext())
        {
            throw check.failure("`module` declaration not at top-level");
        }

        Evaluator eval = expander.getEvaluator();
        ModuleInstance kernel = expander.getKernel();

        // TODO precompute this?
        IdentityHashMap<Binding, Object> stops =
            new IdentityHashMap<Binding, Object>();
        Binding defineBinding       = stopBinding(kernel, stops, "define");
        Binding defineSyntaxBinding = stopBinding(kernel, stops, "define_syntax");
        Binding useSyntaxBinding    = stopBinding(kernel, stops, "use");
        Binding beginBinding        = stopBinding(kernel, stops, "begin");

        SyntaxSymbol moduleNameSymbol = check.requiredSymbol("module name", 1);
        ModuleIdentity.validateLocalName(moduleNameSymbol);

        ModuleRegistry registry = envOutsideModule.namespace().getRegistry();

        ModuleIdentity initialBindingsId;
        ModuleInstance language;
        {
            SyntaxValue initialBindingsStx =
                check.requiredForm("initial module path", 2);
            try
            {
                initialBindingsId =
                    myModuleNameResolver.resolve(eval, initialBindingsStx);
                language = registry.lookup(initialBindingsId);
                assert language != null;  // Otherwise resolve should fail
            }
            catch (ModuleNotFoundFailure e)
            {
                e.addContext(initialBindingsStx);
                throw e;
            }
            catch (FusionException e)
            {
                String message =
                    "Error installing initial bindings: " + e.getMessage();
                SyntaxFailure ex =
                    new SyntaxFailure(getInferredName(), message,
                                      initialBindingsStx);
                ex.initCause(e);
                throw ex;
            }
        }

        // The new namespace shares the registry of current-namespace
        ModuleIdentity id;
        try
        {
            id = determineIdentity(eval, moduleNameSymbol);
        }
        catch (FusionException e)
        {
            String message =
                "Error determining module identity: " + e.getMessage();
            SyntaxFailure ex = check.failure(message);
            ex.initCause(e);
            throw ex;
        }

        Namespace moduleNamespace =
            new ModuleNamespace(registry, language, id);

        // TODO handle #%module-begin and #%plain-module-begin
        expander = expander.enterModuleContext();


        // Pass 1: locate definitions and install dummy bindings

        // TODO FUSION-38 Imported names scope should be the whole module.
        // Probably need to have the module-level wrap extended with the
        // imports, so we don't have to backtrack and add more wraps to
        // earlier forms.  Or maybe scan for the imports first?

        ArrayList<SyntaxSexp> provideForms = new ArrayList<SyntaxSexp>();
        ArrayList<SyntaxValue> otherForms = new ArrayList<SyntaxValue>();
        ArrayList<Boolean> preparedFormFlags = new ArrayList<Boolean>();

        LinkedList<SyntaxValue> forms = new LinkedList<SyntaxValue>();
        source.extract(forms, 3);

        int formsAlreadyWrapped = 0;
        while (! forms.isEmpty())
        {
            SyntaxValue form = forms.pop();
            if (formsAlreadyWrapped == 0)
            {
                // This is ugly, but we don't want to do this twice. Forms
                // that were spliced by 'begin' have already had this done.
                form = moduleNamespace.syntaxIntroduce(form);
            }
            else
            {
                formsAlreadyWrapped--;
            }

            SyntaxSexp provide = formIsProvide(form);
            if (provide != null)
            {
                provideForms.add(provide);
            }
            else
            {
                boolean formIsPrepared = false;
                SyntaxValue expanded;
                if (form instanceof SyntaxSexp)
                {
                    expanded =
                        ((SyntaxSexp)form).partialExpand(expander,
                                                         moduleNamespace,
                                                         stops);
                    if (expanded instanceof SyntaxSexp)
                    {
                        SyntaxSexp sexp = (SyntaxSexp)expanded;
                        Binding binding = firstBinding(sexp);

                        if (binding == defineBinding)
                        {
                            SyntaxSymbol identifier =
                                DefineForm.boundIdentifier(expander.getEvaluator(),
                                                           moduleNamespace,
                                                           sexp);
                            moduleNamespace.predefine(identifier, form);
                        }
                        else if (binding == defineSyntaxBinding)
                        {
                            try
                            {
                                expanded =
                                    expander.expand(moduleNamespace, expanded);
                                // TODO this is getting compiled twice
                                CompiledForm compiled =
                                    eval.compile(moduleNamespace, expanded);
                                eval.eval(moduleNamespace, compiled);
                            }
                            catch (FusionException e)
                            {
                                String message = e.getMessage();
                                throw new SyntaxFailure("define_syntax",
                                                        message, form);
                            }
                            formIsPrepared = true;
                        }
                        else if (binding == useSyntaxBinding)
                        {
                            try
                            {
                                evalSyntax(eval, expanded, moduleNamespace);
                            }
                            catch (FusionException e)
                            {
                                String message = e.getMessage();
                                throw new SyntaxFailure("use",
                                                        message, form);
                            }
                            expanded = null;
                        }
                        else if (binding == beginBinding)
                        {
                            // Top-level 'begin' is spliced into the module
                            int last = sexp.size() - 1;
                            for (int i = last; i != 0;  i--)
                            {
                                forms.push(sexp.get(i));
                            }
                            formsAlreadyWrapped += last;
                            expanded = null;
                        }
                    }
                }
                else
                {
                    expanded = form;
                }

                if (expanded != null)
                {
                    otherForms.add(expanded);
                    preparedFormFlags.add(formIsPrepared);
                }
            }
        }

        // Pass 2: Expand the expressions. We also rearrange the forms,
        // but that's not really for any functional reason.

        SyntaxValue[] subforms =
            new SyntaxValue[4 + otherForms.size() + provideForms.size()];

        int i = 0;
        subforms[i++] = source.get(0); // module
        subforms[i++] = source.get(1); // name
        subforms[i++] = source.get(2); // language

        // FIXME a ludicrous hack to communicate this data to the compiler!
        {
            SyntaxInt variableCount =
                SyntaxInt.make(moduleNamespace.getBindings().size());

            SyntaxString identity =
                SyntaxString.make(id.internString());

            SyntaxString languageIdentity =
                SyntaxString.make(initialBindingsId.internString());

            SyntaxStruct meta =
                SyntaxStruct.make(new String[] { "variable_count",
                                                 "identity",
                                                 "language_identity" },
                                  new SyntaxValue[] { variableCount,
                                                      identity,
                                                      languageIdentity },
                                  new String[] { "InjectedMetadata" });

            subforms[i++] = meta;
        }

        Iterator<Boolean> prepared = preparedFormFlags.iterator();
        for (SyntaxValue stx : otherForms)
        {
            if (! prepared.next())
            {
                if (firstBinding(stx) == defineBinding)
                {
                    assert expander.isModuleContext();
                    stx = expander.expand(moduleNamespace, stx);
                }
                else
                {
                    stx = expander.expandExpression(moduleNamespace, stx);
                }
            }
            subforms[i++] = stx;
        }

        for (SyntaxSexp stx : provideForms)
        {
            stx = prepareProvide(stx, moduleNamespace);
            subforms[i++] = stx;
        }

        SyntaxSexp result = SyntaxSexp.make(expander, source.getLocation(),
                                            subforms);
        return result;
    }

    /**
     * Finds the binding for the leading symbol in the form, or null if the
     * form doesn't start with a symbol.
     */
    Binding firstBinding(SyntaxSexp form)
    {
        if (form.size() != 0)
        {
            SyntaxValue first = form.get(0);
            if (first instanceof SyntaxSymbol)
            {
                Binding binding = ((SyntaxSymbol)first).getBinding();
                return binding.originalBinding();
            }
        }
        return null;
    }

    Binding firstBinding(SyntaxValue stx)
    {
        if (stx instanceof SyntaxSexp)
        {
            return firstBinding((SyntaxSexp) stx);
        }
        return null;
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment envOutsideModule,
                         SyntaxSexp stx)
        throws FusionException
    {
        // TODO We repeat work here that was done during expansion.
        // Racket stashes the necessary data in syntax properties.
        // See Racket reference 11.9.1
        // http://docs.racket-lang.org/reference/Expanding_Top-Level_Forms.html#%28part._modinfo%29

        SyntaxStruct meta = (SyntaxStruct) stx.get(3);

        ModuleIdentity id;
        {
            SyntaxText form = (SyntaxText) meta.get(eval, "identity");
            String identity = form.stringValue();
            id = ModuleIdentity.reIntern(identity);
        }

        Namespace moduleNamespace;
        {
            SyntaxText form = (SyntaxText) meta.get(eval, "language_identity");
            String identity = form.stringValue();
            ModuleIdentity languageId = ModuleIdentity.reIntern(identity);

            ModuleRegistry registry =
                envOutsideModule.namespace().getRegistry();
            ModuleInstance language = registry.lookup(languageId);

            moduleNamespace = new ModuleNamespace(registry, language, id);
        }

        int variableCount;
        {
            SyntaxInt form = (SyntaxInt) meta.get(eval, "variable_count");
            variableCount = form.bigIntegerValue().intValue();
        }

        ArrayList<SyntaxSexp> provideForms = new ArrayList<SyntaxSexp>();
        ArrayList<CompiledForm> otherForms = new ArrayList<CompiledForm>();

        int bodyPos = 4;
        String docs = null;
        if (stx.size() > 5 && stx.get(4) instanceof SyntaxString)
        {
            // We're gonna call this documentation!
            bodyPos++;
            if (eval.firstContinuationMark(COLLECT_DOCS_MARK) != null)
            {
                docs = ((SyntaxString) stx.get(4)).stringValue();
            }
        }

        for (int i = bodyPos; i < stx.size(); i++)
        {
            SyntaxValue form = stx.get(i);
            SyntaxSexp provide = formIsProvide(form);
            if (provide != null)
            {
                provideForms.add(provide);
            }
            else
            {
                CompiledForm compiled = eval.compile(moduleNamespace, form);
                otherForms.add(compiled);
            }
        }

        SyntaxSymbol[] providedIdentifiers = providedSymbols(provideForms);

        CompiledForm[] otherFormsArray =
            otherForms.toArray(new CompiledForm[otherForms.size()]);

        return new CompiledModule(id,
                                  docs,
                                  moduleNamespace.requiredModuleIds(),
                                  variableCount,
                                  moduleNamespace.extractBindingDocs(),
                                  providedIdentifiers, otherFormsArray);
    }

    private ModuleIdentity determineIdentity(Evaluator eval,
                                             SyntaxSymbol moduleNameSymbol)
        throws FusionException
    {
        ModuleIdentity id;
        String current = myCurrentModuleDeclareName.asString(eval);
        if (current != null)
        {
            id = ModuleIdentity.reIntern(current);
        }
        else
        {
            String declaredName = moduleNameSymbol.stringValue();
            id = ModuleIdentity.internLocalName(declaredName);
        }
        return id;
    }


    //========================================================================


    private SyntaxSexp formIsProvide(SyntaxValue form)
    {
        if (form.getType() == SyntaxValue.Type.SEXP)
        {
            SyntaxSexp sexp = (SyntaxSexp) form;
            if (sexp.size() != 0)
            {
                SyntaxValue first = sexp.get(0);
                if (first.getType() == SyntaxValue.Type.SYMBOL
                    && "provide".equals(((SyntaxSymbol) first).stringValue()))
                {
                    return sexp;
                }
            }
        }
        return null;
    }

    /**
     * @param provideForms
     * @param myNamespace
     * @return
     */
    private SyntaxSexp prepareProvide(SyntaxSexp form,
                                      Namespace moduleNamespace)
        throws SyntaxFailure
    {
        int size = form.size();
        SyntaxChecker check = new SyntaxChecker("provide", form);
        for (int i = 1; i < size; i++)
        {
            check.requiredIdentifier("bound identifier", i);

            SyntaxSymbol identifier = (SyntaxSymbol) form.get(i);
            String publicName = identifier.stringValue();

            Binding b = identifier.resolve();
            if (b instanceof FreeBinding)
            {
                String message =
                    "cannot export " + printQuotedSymbol(publicName) +
                    " since it has no definition.";
                throw check.failure(message);
            }

            String freeName = b.getName();
            if (! publicName.equals(freeName))
            {
                String message =
                    "cannot export binding since symbolic name " +
                    printQuotedSymbol(publicName) +
                    " differs from resolved name " +
                    printQuotedSymbol(freeName);
                throw check.failure(message);
            }
        }

        return form;
    }


    /**
     * @return not null.
     */
    private SyntaxSymbol[] providedSymbols(ArrayList<SyntaxSexp> provideForms)
        throws SyntaxFailure
    {
        ArrayList<SyntaxSymbol> identifiers = new ArrayList<SyntaxSymbol>();

        for (SyntaxSexp form : provideForms)
        {
            int size = form.size();
            for (int i = 1; i < size; i++)
            {
                SyntaxSymbol identifier = (SyntaxSymbol) form.get(i);
                identifiers.add(identifier);
            }
        }

        return identifiers.toArray(new SyntaxSymbol[identifiers.size()]);
    }


    //========================================================================


    private static final class CompiledModule
        implements CompiledForm
    {
        private final ModuleIdentity   myId;
        private final String           myDocs;
        private final ModuleIdentity[] myRequiredModules;
        private final int              myVariableCount;
        private final BindingDoc[]     myBindingDocs;
        private final SyntaxSymbol[]   myProvidedIdentifiers;
        private final CompiledForm[]   myBody;

        private CompiledModule(ModuleIdentity   id,
                               String           docs,
                               ModuleIdentity[] requiredModules,
                               int              variableCount,
                               BindingDoc[]     bindingDocs,
                               SyntaxSymbol[]   providedIdentifiers,
                               CompiledForm[]   body)
        {
            myId                  = id;
            myDocs                = docs;
            myRequiredModules     = requiredModules;
            myVariableCount       = variableCount;
            myBindingDocs         = bindingDocs;
            myProvidedIdentifiers = providedIdentifiers;
            myBody                = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store ignored)
            throws FusionException
        {
            ModuleRegistry registry =
                eval.findCurrentNamespace().getRegistry();

            ModuleStore[] requiredModuleStores = requireModules(registry);

            // Allocate just enough space for the top-level bindings.
            ModuleStore store =
                new ModuleStore(registry, requiredModuleStores,
                                myVariableCount, myBindingDocs);

            ModuleInstance module =
                new ModuleInstance(myId, myDocs, store, myProvidedIdentifiers);

            eval.findCurrentNamespace().getRegistry().register(module);

            for (CompiledForm form : myBody)
            {
                eval.eval(store, form);
            }

            return module;
        }

        private ModuleStore[] requireModules(ModuleRegistry registry)
        {
            int count = myRequiredModules.length;
            ModuleStore[] stores = new ModuleStore[count];
            for (int i = 0; i < count; i++)
            {
                ModuleInstance module = registry.lookup(myRequiredModules[i]);
                stores[i] = module.getNamespace();
            }

            return stores;
        }
    }
}
