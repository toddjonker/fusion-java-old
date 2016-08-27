// Copyright (c) 2012-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BindingDoc.COLLECT_DOCS_MARK;
import static com.amazon.fusion.FusionString.isString;
import static com.amazon.fusion.FusionString.stringToJavaString;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.GlobalState.PROVIDE;
import static com.amazon.fusion.GlobalState.REQUIRE;
import static com.amazon.fusion.ModuleIdentity.isValidAbsoluteModulePath;
import static com.amazon.fusion.ModuleIdentity.isValidModulePath;
import static com.amazon.ion.util.IonTextUtils.printQuotedSymbol;
import com.amazon.fusion.FusionSymbol.BaseSymbol;
import com.amazon.fusion.ModuleNamespace.ProvidedBinding;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @see <a href="http://docs.racket-lang.org/reference/module.html">Racket
 * modules</a>
 */
final class ModuleForm
    extends SyntacticForm
{
    private static final String STX_PROP_MODULE_IDENTITY   = "module identity";
    private static final String STX_PROP_LANGUAGE_IDENTITY = "language identity";
    private static final String STX_PROP_DEFINITION_COUNT  = "definition count";


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

        // Ensure there's a name and language. We check for a body below, but
        // want a different error message for that situation.
        if (source.size() < 3)
        {
            throw check.failure("Malformed module declaration");
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
        source = (SyntaxSexp)
            source.copyWithProperty(eval, STX_PROP_MODULE_IDENTITY, id);

        ModuleInstance language;
        {
            String path = check.requiredText(eval, "initial module path", 2);
            SyntaxValue initialBindingsStx = source.get(eval, 2);

            if (! isValidModulePath(path))
            {
                String message = "Invalid module path for language: "
                    + initialBindingsStx.toString();
                throw new ModuleNotFoundException(message, initialBindingsStx);
            }
            if (! isValidAbsoluteModulePath(path))
            {
                String message = "Module path for language must be absolute: "
                    + initialBindingsStx.toString();
                throw new ModuleNotFoundException(message, initialBindingsStx);
            }

            ModuleIdentity languageId;
            try
            {
                languageId =
                    myModuleNameResolver.resolve(eval, id, initialBindingsStx,
                                                 true /*load it*/);
                language = registry.instantiate(eval, languageId);
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
                    "Error installing language bindings: " + e.getMessage();
                SyntaxException ex =
                    new SyntaxException(getInferredName(), message,
                                        initialBindingsStx);
                ex.initCause(e);
                throw ex;
            }

            source = (SyntaxSexp)
                source.copyWithProperty(eval,
                                        STX_PROP_LANGUAGE_IDENTITY, languageId);
        }


        if (source.size() < 4)
        {
            throw check.failure("Module has no body");
        }

        // The new namespace shares the registry of current-namespace.
        // We use the module-name symbol as lexical context for bindings
        // introduced by the language, so they "match" references in the body
        // of the module. The leading 'module' symbol is enriched differently
        // when this is called via `eval` and top-level evaluation.
        ModuleNamespace moduleNamespace =
            new ModuleNamespace(eval, registry,
                                (SyntaxSymbol) source.get(eval, 1),
                                language, id);

        // TODO handle #%module-begin and #%plain-module-begin
        expander = expander.enterModuleContext();


        // Pass 1: locate definitions and install dummy bindings

        // TODO FUSION-38 Imported names scope should be the whole module.
        // Probably need to have the module-level wrap extended with the
        // imports, so we don't have to backtrack and add more wraps to
        // earlier forms.  Or maybe scan for the imports first?

        ArrayList<SyntaxSexp>  provideForms      = new ArrayList<>();
        ArrayList<SyntaxValue> otherForms        = new ArrayList<>();
        ArrayList<Boolean>     expandedFormFlags = new ArrayList<>();

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

            boolean formIsExpanded = false;

            SyntaxValue expanded =
                expander.partialExpand(moduleNamespace, form);

            if (expanded instanceof SyntaxSexp)
            {
                SyntaxSexp sexp = (SyntaxSexp)expanded;
                Binding binding = sexp.firstTargetBinding(eval);

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
                    formIsExpanded = true;
                }
                else if (binding == globals.myKernelRequireBinding)
                {
                    try
                    {
                        expanded = expander.expand(moduleNamespace, expanded);
                        CompiledForm compiled =
                            eval.compile(moduleNamespace, expanded);
                        eval.eval(moduleNamespace, compiled);
                    }
                    catch (FusionException e)
                    {
                        String message = e.getMessage();
                        SyntaxException ex =
                            new SyntaxException(REQUIRE, message, form);
                        ex.initCause(e);
                        throw ex;
                    }
                    formIsExpanded = true;
                }
                else if (binding == globals.myKernelProvideBinding)
                {
                    provideForms.add(sexp);
                    expanded = null;
                }
                else if (binding == globals.myKernelBeginBinding)
                {
                    // Splice 'begin' into the module-begin sequence
                    SyntaxSymbol beginId = (SyntaxSymbol) sexp.get(eval, 0);
                    int last = sexp.size() - 1;
                    for (int i = last; i != 0;  i--)
                    {
                        SyntaxValue expr = sexp.get(eval, i);
                        expr = expr.trackOrigin(eval, expanded, beginId);
                        forms.push(expr);
                    }
                    formsAlreadyWrapped += last;
                    expanded = null;
                }
            }

            if (expanded != null)
            {
                otherForms.add(expanded);
                expandedFormFlags.add(formIsExpanded);
            }
        }

        source = (SyntaxSexp)
            source.copyWithProperty(eval,
                                    STX_PROP_DEFINITION_COUNT,
                                    moduleNamespace.definitionCount());


        // Pass 2: Expand the expressions. We also rearrange the forms,
        // but that's not really for any functional reason.

        SyntaxValue[] subforms =
            new SyntaxValue[3 + otherForms.size() + provideForms.size()];

        int i = 0;
        subforms[i++] = source.get(eval, 0); // module
        subforms[i++] = source.get(eval, 1); // name
        subforms[i++] = source.get(eval, 2); // language

        Iterator<Boolean> expanded = expandedFormFlags.iterator();
        for (SyntaxValue stx : otherForms)
        {
            if (! expanded.next())
            {
                Binding firstBinding = firstTargetBindingOfSexp(eval, stx);
                if (firstBinding == globals.myKernelDefineBinding)
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

        // Push all the `provide` forms to the end of the module.
        for (SyntaxValue stx : provideForms)
        {
            stx = expander.expand(moduleNamespace, stx);
            subforms[i++] = stx;
        }

        return source.copyReplacingChildren(eval, subforms);
    }

    /**
     * @return null if stx isn't a sexp or doesn't start with an identifier.
     * Null is also equivalent to a {@link FreeBinding} on a lead identifier.
     */
    private static Binding firstTargetBindingOfSexp(Evaluator eval,
                                                    SyntaxValue stx)
        throws FusionException
    {
        if (stx instanceof SyntaxSexp)
        {
            return ((SyntaxSexp) stx).firstTargetBinding(eval);
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

        ModuleIdentity id = (ModuleIdentity)
            stx.findProperty(eval, STX_PROP_MODULE_IDENTITY);

        Namespace moduleNamespace;
        {
            ModuleIdentity languageId = (ModuleIdentity)
                stx.findProperty(eval, STX_PROP_LANGUAGE_IDENTITY);

            ModuleRegistry registry =
                envOutsideModule.namespace().getRegistry();
            ModuleInstance language = registry.instantiate(eval, languageId);

            moduleNamespace = new ModuleNamespace(eval, registry,
                                                  (SyntaxSymbol) stx.get(eval, 0),
                                                  language, id);
        }


        ArrayList<CompiledForm> otherForms = new ArrayList<>();

        int bodyPos = 3;
        String docs = null;
        if (stx.size() > bodyPos + 1)
        {
            Object maybeDoc = stx.get(eval, bodyPos).unwrap(eval);
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


        final Binding kernelRequireBinding =
            eval.getGlobalState().myKernelRequireBinding;
        final Binding kernelProvideBinding =
            eval.getGlobalState().myKernelProvideBinding;

        int i;
        for (i = bodyPos; i < stx.size(); i++)
        {
            SyntaxValue form = stx.get(eval, i);

            Binding firstBinding = firstTargetBindingOfSexp(eval, form);
            if (firstBinding == kernelRequireBinding)
            {
                // All require forms have already been evaluated.
                // We preserve them through macro expansion so that the are
                // retained by `expand` and similar operations.
                continue;
            }

            if (firstBinding == kernelProvideBinding)
            {
                // Expansion pushes all `provide` forms to the end of the
                // module, so when we hit one we know we're done compiling.
                break;
            }

            CompiledForm compiled = eval.compile(moduleNamespace, form);
            otherForms.add(compiled);
        }

        ProvidedBinding[] providedBindings =
            providedBindings(eval, moduleNamespace, stx, i);

        CompiledForm[] otherFormsArray =
            otherForms.toArray(CompiledForm.EMPTY_ARRAY);

        int bindingCount = (Integer)
            stx.findProperty(eval, STX_PROP_DEFINITION_COUNT);

        return new CompiledModule(id,
                                  docs,
                                  moduleNamespace.requiredModuleIds(),
                                  bindingCount,
                                  moduleNamespace.extractBindingDocs(),
                                  providedBindings,
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
     * Process all the provide-forms, which macro-expansion has grouped
     * together at the end of the module.
     *
     * @param firstProvidePos the index within {@code moduleStx} of the first
     * provide-form. All elements from that position onward are provide-forms.
     */
    private ProvidedBinding[] providedBindings(Evaluator  eval,
                                               Namespace  ns,
                                               SyntaxSexp moduleStx,
                                               int        firstProvidePos)
        throws FusionException
    {
        Map<BaseSymbol,Binding> bound    = new IdentityHashMap<>();
        List<ProvidedBinding>   bindings = new ArrayList<>();

        for (int p = firstProvidePos; p < moduleStx.size(); p++)
        {
            SyntaxSexp form = (SyntaxSexp) moduleStx.get(eval, p);
            SyntaxChecker check = new SyntaxChecker(eval, PROVIDE, form);

            int size = form.size();
            for (int i = 1; i < size; i++)
            {
                SyntaxSymbol exportId;
                Binding binding;

                SyntaxValue spec = form.get(eval, i);
                if (spec instanceof SyntaxSymbol)
                {
                    exportId = (SyntaxSymbol) spec;
                    binding = exportId.getBinding();
                }
                else
                {
                    SyntaxSexp sexp = (SyntaxSexp) spec;
                    SyntaxSymbol formName = sexp.firstIdentifier(eval);
                    switch (formName.getName().stringValue())
                    {
                        case "rename":
                        {
                            SyntaxSymbol localId = (SyntaxSymbol)
                                sexp.get(eval, 1);
                            exportId = (SyntaxSymbol) sexp.get(eval, 2);
                            binding = localId.getBinding();
                            break;
                        }
                        default:
                        {
                            throw check.failure("invalid provide-spec");
                        }
                    }
                }

                BaseSymbol name = exportId.getName();
                Binding prior = bound.put(name, binding);
                if (prior != null && ! binding.sameTarget(prior))
                {
                    String message =
                        "the identifier " +
                        printQuotedSymbol(name.stringValue()) +
                        " is being exported with multiple bindings";
                    throw check.failure(message, exportId);
                }

                ProvidedBinding provided = binding.provideAs(name);
                assert name == provided.getName();

                bindings.add(provided);
            }
        }

        return bindings.toArray(new ProvidedBinding[0]);
    }


    //========================================================================


    static final class CompiledModule
        implements CompiledForm
    {
        // We use arrays to hold the provided bindings, rather than a map
        // from names to bindings, because we want compiled forms to be flat
        // and compact.
        private final ModuleIdentity    myId;
        private final String            myDocs;
        private final ModuleIdentity[]  myRequiredModules;
        private final int               myVariableCount;
        private final BindingDoc[]      myBindingDocs;
        private final ProvidedBinding[] myProvidedBindings;
        private final CompiledForm[]    myBody;

        private CompiledModule(ModuleIdentity    id,
                               String            docs,
                               ModuleIdentity[]  requiredModules,
                               int               variableCount,
                               BindingDoc[]      bindingDocs,
                               ProvidedBinding[] providedBindings,
                               CompiledForm[]    body)
        {
            myId                  = id;
            myDocs                = docs;
            myRequiredModules     = requiredModules;
            myVariableCount       = variableCount;
            myBindingDocs         = bindingDocs;
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
                new ModuleInstance(myId, myDocs, store, myProvidedBindings);

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
