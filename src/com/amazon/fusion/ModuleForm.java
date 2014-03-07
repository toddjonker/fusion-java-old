// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BindingDoc.COLLECT_DOCS_MARK;
import static com.amazon.fusion.FusionEval.evalSyntax;
import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionNumber.unsafeTruncateIntToJavaInt;
import static com.amazon.fusion.FusionString.isString;
import static com.amazon.fusion.FusionString.makeString;
import static com.amazon.fusion.FusionString.stringToJavaString;
import static com.amazon.fusion.FusionStruct.immutableStruct;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.GlobalState.PROVIDE;
import static com.amazon.fusion.GlobalState.REQUIRE;
import static com.amazon.fusion.ModuleIdentity.isValidAbsoluteModulePath;
import static com.amazon.fusion.SimpleSyntaxValue.makeSyntax;
import com.amazon.fusion.ModuleNamespace.ModuleBinding;
import java.util.ArrayList;
import java.util.HashMap;
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
        super("name language body ...+",
              "Declares a module containing the given body. The `name` must be a symbol; it is\n" +
              "ignored when loading a module from a file.\n" +
              "\n" +
              "The `language` must be an absolute [module path][]. The denoted module is\n" +
              "instantiated and all of its `provide`d bindings are immediately imported.  This\n" +
              "\"bootstraps\" the module with a set of bindings that form the base semantics\n" +
              "for the body.  Unlike bindings that are `require`d, these bindings can be\n" +
              "shadowed by module-level definitions and by `require` statements.\n" +
              "\n" +
              "When compiling a module, the `body` forms are partially macro-expanded in\n" +
              "order to discover certain core forms like `require` and `provide`.  The former\n" +
              "are handled immediately, before other forms.  The latter are handled _after_\n" +
              "all other forms.\n" +
              "At module level, the elements within `begin` forms are spliced into the\n" +
              "enclosing module body, replacing the single `begin` form with its elements.\n" +
              "This effectively enables module-level macro uses to expand into multiple forms.\n" +
              "\n" +
              "[module path]: module.html#ref");

        myCurrentModuleDeclareName = currentModuleDeclareName;
        myModuleNameResolver = moduleNameResolver;
    }


    @Override
    SyntaxValue expand(Expander expander, Environment envOutsideModule,
                       SyntaxSexp source)
        throws FusionException
    {
        final Evaluator   eval    = expander.getEvaluator();
        final GlobalState globals = eval.getGlobalState();

        SyntaxChecker check = check(eval, source);
        if (! expander.isTopLevelContext())
        {
            throw check.failure("`module` declaration not at top-level");
        }



        SyntaxSymbol moduleNameSymbol =
            check.requiredIdentifier("module name", 1);
        ModuleIdentity.validateLocalName(moduleNameSymbol);

        ModuleRegistry registry = envOutsideModule.namespace().getRegistry();

        ModuleIdentity id;
        try
        {
            id = determineIdentity(eval, envOutsideModule, moduleNameSymbol);
        }
        catch (FusionException e)
        {
            String message =
                "Error determining module identity: " + e.getMessage();
            SyntaxException ex = check.failure(message);
            ex.initCause(e);
            throw ex;
        }

        ModuleIdentity initialBindingsId;
        ModuleInstance language;
        {
            String path = check.requiredText(eval, "initial module path", 2);
            SyntaxValue initialBindingsStx = source.get(eval, 2);

            if (! isValidAbsoluteModulePath(path))
            {
                String message = "Module path for language must be absolute";
                throw new ModuleNotFoundException(message, initialBindingsStx);
            }

            try
            {
                initialBindingsId =
                    myModuleNameResolver.resolve(eval, id, initialBindingsStx,
                                                 true /*load it*/);
                language = registry.instantiate(eval, initialBindingsId);
                assert language != null;  // Otherwise resolve should fail
            }
            catch (ModuleNotFoundException e)
            {
                e.addContext(initialBindingsStx);
                throw e;
            }
            catch (FusionException e)
            {
                String message =
                    "Error installing initial bindings: " + e.getMessage();
                SyntaxException ex =
                    new SyntaxException(getInferredName(), message,
                                        initialBindingsStx);
                ex.initCause(e);
                throw ex;
            }
        }

        // The new namespace shares the registry of current-namespace
        ModuleNamespace moduleNamespace =
            new ModuleNamespace(registry, language, id);

        // TODO handle #%module-begin and #%plain-module-begin
        expander = expander.enterModuleContext();


        // Pass 1: locate definitions and install dummy bindings

        // TODO FUSION-38 Imported names scope should be the whole module.
        // Probably need to have the module-level wrap extended with the
        // imports, so we don't have to backtrack and add more wraps to
        // earlier forms.  Or maybe scan for the imports first?

        ArrayList<SyntaxSexp>  provideForms      = new ArrayList<>();
        ArrayList<SyntaxValue> otherForms        = new ArrayList<>();
        ArrayList<Boolean>     preparedFormFlags = new ArrayList<>();

        LinkedList<SyntaxValue> forms = new LinkedList<>();
        source.extract(eval, forms, 3);

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

            boolean formIsPrepared = false;

            SyntaxValue expanded =
                expander.partialExpand(moduleNamespace, form);

            if (expanded instanceof SyntaxSexp)
            {
                SyntaxSexp sexp = (SyntaxSexp)expanded;
                Binding binding = sexp.firstBinding(eval);

                if (binding == globals.myKernelDefineBinding)
                {
                    expanded = DefineForm.predefine(eval, moduleNamespace,
                                                    sexp, form);
                }
                else if (binding == globals.myKernelDefineSyntaxBinding)
                {
                    try
                    {
                        expanded = expander.expand(moduleNamespace, expanded);
                        // TODO this is getting compiled twice
                        CompiledForm compiled =
                            eval.compile(moduleNamespace, expanded);
                        eval.eval(moduleNamespace, compiled);
                    }
                    catch (FusionException e)
                    {
                        e.addContext(form);
                        throw e;
                    }
                    formIsPrepared = true;
                }
                else if (binding == globals.myKernelRequireBinding)
                {
                    try
                    {
                        evalSyntax(eval, expanded, moduleNamespace);
                    }
                    catch (FusionException e)
                    {
                        String message = e.getMessage();
                        SyntaxException ex =
                            new SyntaxException(REQUIRE, message, form);
                        ex.initCause(e);
                        throw ex;
                    }
                    expanded = null;
                }
                else if (binding == globals.myKernelProvideBinding)
                {
                    provideForms.add(sexp);
                    expanded = null;
                }
                else if (binding == globals.myKernelBeginBinding)
                {
                    // Splice 'begin' into the module-begin sequence
                    int last = sexp.size() - 1;
                    for (int i = last; i != 0;  i--)
                    {
                        forms.push(sexp.get(eval, i));
                    }
                    formsAlreadyWrapped += last;
                    expanded = null;
                }
            }

            if (expanded != null)
            {
                otherForms.add(expanded);
                preparedFormFlags.add(formIsPrepared);
            }
        }

        // Pass 2: Expand the expressions. We also rearrange the forms,
        // but that's not really for any functional reason.

        SyntaxValue[] subforms =
            new SyntaxValue[4 + otherForms.size() + provideForms.size()];

        int i = 0;
        subforms[i++] = source.get(eval, 0); // module
        subforms[i++] = source.get(eval, 1); // name
        subforms[i++] = source.get(eval, 2); // language

        // FIXME a ludicrous hack to communicate this data to the compiler!
        {
            BaseValue datum =
                makeInt(eval, moduleNamespace.getBindings().size());
            SyntaxValue variableCount =
                makeSyntax(eval, /*location*/ null, datum);

            SyntaxValue identity =
                makeString(eval, id.absolutePath())
                .wrapAsSyntax(eval, null);

            SyntaxValue languageIdentity =
                makeString(eval, initialBindingsId.absolutePath())
                .wrapAsSyntax(eval, null);

            Object s =
                immutableStruct(new String[] { "variable_count",
                                               "identity",
                                               "language_identity" },
                                new SyntaxValue[] { variableCount,
                                                    identity,
                                                    languageIdentity },
                                new String[] { "InjectedMetadata" });
            subforms[i++] = SyntaxStruct.make(eval, null, s);
        }

        Iterator<Boolean> prepared = preparedFormFlags.iterator();
        for (SyntaxValue stx : otherForms)
        {
            if (! prepared.next())
            {
                if (firstBinding(eval, stx) == globals.myKernelDefineBinding)
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

        for (SyntaxValue stx : provideForms)
        {
            stx = expander.expand(moduleNamespace, stx);
            subforms[i++] = stx;
        }

        SyntaxSexp result = SyntaxSexp.make(eval, source.getLocation(),
                                            subforms);
        return result;
    }

    Binding firstBinding(Evaluator eval, SyntaxValue stx)
        throws FusionException
    {
        if (stx instanceof SyntaxSexp)
        {
            return ((SyntaxSexp) stx).firstBinding(eval);
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

        SyntaxStruct meta = (SyntaxStruct) stx.get(eval, 3);

        ModuleIdentity id;
        {
            Object datum = meta.get(eval, "identity").unwrap(eval);
            String identity = stringToJavaString(eval, datum);
            id = ModuleIdentity.reIntern(identity);
        }

        Namespace moduleNamespace;
        {
            Object datum = meta.get(eval, "language_identity").unwrap(eval);
            String identity = stringToJavaString(eval, datum);
            ModuleIdentity languageId =
                ModuleIdentity.forAbsolutePath(identity);

            ModuleRegistry registry =
                envOutsideModule.namespace().getRegistry();
            ModuleInstance language = registry.instantiate(eval, languageId);

            moduleNamespace = new ModuleNamespace(registry, language, id);
        }

        int variableCount;
        {
            Object i = meta.get(eval, "variable_count").unwrap(eval);
            variableCount = unsafeTruncateIntToJavaInt(eval, i);
        }

        ArrayList<CompiledForm> otherForms = new ArrayList<>();

        int bodyPos = 4;
        String docs = null;
        if (stx.size() > 5)
        {
            Object maybeDoc = stx.get(eval, 4).unwrap(eval);
            if (isString(eval, maybeDoc))
            {
                // We're gonna call this documentation!
                bodyPos++;
                if (eval.firstContinuationMark(COLLECT_DOCS_MARK) != null)
                {
                    docs = stringToJavaString(eval, maybeDoc);
                }
            }
        }


        final Binding kernelProvideBinding =
            eval.getGlobalState().myKernelProvideBinding;

        int i;
        for (i = bodyPos; i < stx.size(); i++)
        {
            SyntaxValue form = stx.get(eval, i);
            if (firstBinding(eval, form) == kernelProvideBinding)
            {
                break;
            }

            CompiledForm compiled = eval.compile(moduleNamespace, form);
            otherForms.add(compiled);
        }

        Object[] providedIdentifiers =
            providedSymbols(eval, moduleNamespace, stx, i);

        CompiledForm[] otherFormsArray =
            otherForms.toArray(new CompiledForm[otherForms.size()]);

        return new CompiledModule(id,
                                  docs,
                                  moduleNamespace.requiredModuleIds(),
                                  variableCount,
                                  moduleNamespace.extractBindingDocs(),
                                  (String[]) providedIdentifiers[0],
                                  (ModuleBinding[]) providedIdentifiers[1],
                                  otherFormsArray);
    }

    private ModuleIdentity determineIdentity(Evaluator eval,
                                             Environment envOutsideModule,
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
            Namespace outsideNs = envOutsideModule.namespace();

            // current_namespace should be the top-level we're evaluating in.
            assert outsideNs == eval.findCurrentNamespace();

            String declaredName = moduleNameSymbol.stringValue();
            id = ModuleIdentity.forLocalName(outsideNs, declaredName);
        }
        return id;
    }


    //========================================================================


    /**
     * @return [String[] names, ModuleBinding[] bindings]
     */
    private Object[] providedSymbols(Evaluator  eval,
                                     Namespace  ns,
                                     SyntaxSexp moduleStx,
                                     int        firstProvidePos)
        throws FusionException
    {
        Map<String,Binding>      bound    = new HashMap<>();
        ArrayList<String>        names    = new ArrayList<>();
        ArrayList<ModuleBinding> bindings = new ArrayList<>();

        for (int p = firstProvidePos; p < moduleStx.size(); p++)
        {
            SyntaxSexp form = (SyntaxSexp) moduleStx.get(eval, p);
            SyntaxChecker check = new SyntaxChecker(eval, PROVIDE, form);

            int size = form.size();
            for (int i = 1; i < size; i++)
            {
                SyntaxValue spec = form.get(eval, i);

                SyntaxSymbol id;
                try
                {
                    id = (SyntaxSymbol) spec;
                }
                catch (ClassCastException e)
                {
                    throw check.failure("invalid provide-spec");
                }

                Binding binding = id.getBinding();
                String name = id.stringValue();
                Binding prior = bound.put(name, binding);
                if (prior != null && ! binding.sameTarget(prior))
                {
                    String message =
                        "identifier already provided with a different" +
                        " binding";
                    throw check.failure(message, id);
                }
                names.add(name);
                bindings.add((ModuleBinding) binding.originalBinding());
            }
        }

        int count = names.size();
        Object[] result = { names.toArray(new String[count]),
                            bindings.toArray(new ModuleBinding[count]) };
        return result;
    }


    //========================================================================


    static final class CompiledModule
        implements CompiledForm
    {
        private final ModuleIdentity   myId;
        private final String           myDocs;
        private final ModuleIdentity[] myRequiredModules;
        private final int              myVariableCount;
        private final BindingDoc[]     myBindingDocs;
        private final String[]         myProvidedNames;
        private final ModuleBinding[]  myProvidedBindings;
        private final CompiledForm[]   myBody;

        private CompiledModule(ModuleIdentity   id,
                               String           docs,
                               ModuleIdentity[] requiredModules,
                               int              variableCount,
                               BindingDoc[]     bindingDocs,
                               String[]         providedNames,
                               ModuleBinding[]  providedBindings,
                               CompiledForm[]   body)
        {
            myId                  = id;
            myDocs                = docs;
            myRequiredModules     = requiredModules;
            myVariableCount       = variableCount;
            myBindingDocs         = bindingDocs;
            myProvidedNames       = providedNames;
            myProvidedBindings    = providedBindings;
            myBody                = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store ignored)
            throws FusionException
        {
            ModuleRegistry registry =
                eval.findCurrentNamespace().getRegistry();
            registry.declare(myId, this);

            ModuleNameResolver resolver =
                eval.getGlobalState().myModuleNameResolver;
            resolver.registerDeclaredModule(registry, myId);

            return voidValue(eval);
        }


        ModuleInstance instantiate(Evaluator eval)
            throws FusionException
        {
            ModuleRegistry registry =
                eval.findCurrentNamespace().getRegistry();

            ModuleStore[] requiredModuleStores = requireModules(eval, registry);

            // Allocate just enough space for the top-level bindings.
            ModuleStore store =
                new ModuleStore(registry, requiredModuleStores,
                                myVariableCount, myBindingDocs);

            ModuleInstance module =
                new ModuleInstance(myId, myDocs, store, myProvidedNames,
                                   myProvidedBindings);

            for (CompiledForm form : myBody)
            {
                // TODO FUSION-213 each eval should be wrapped with a prompt.
                // See Racket reference for `module`.
                eval.eval(store, form);
            }

            return module;
        }

        private ModuleStore[] requireModules(Evaluator eval,
                                             ModuleRegistry registry)
            throws FusionException
        {
            int count = myRequiredModules.length;
            ModuleStore[] stores = new ModuleStore[count];
            for (int i = 0; i < count; i++)
            {
                ModuleInstance module =
                    registry.instantiate(eval, myRequiredModules[i]);
                stores[i] = module.getNamespace();
            }

            return stores;
        }
    }
}
