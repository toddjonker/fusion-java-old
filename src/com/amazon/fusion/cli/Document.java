// Copyright (c) 2005-2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion._Private_ModuleDocumenter.documentModule;
import com.amazon.fusion.FusionException;
import com.amazon.fusion.FusionRuntime;
import com.amazon.fusion.FusionRuntimeBuilder;
import com.petebevin.markdown.MarkdownProcessor;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;


class Document
    extends Command
{
    //=+===============================================================================
    private static final String HELP_ONE_LINER =
        "Generate reference documentation for a repository.";

    private static final String HELP_USAGE =
        "document OUTPUT_DIR REPO_DIR";

    private static final String HELP_BODY =
        "Given a REPO_DIR directory containing Fusion source code, generate reference\n" +
        "documentation (in MarkDown format) into the OUTPUT_DIR.";


    //=========================================================================
    // Constructors

    Document()
    {
        super("document");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    //=========================================================================


    @Override
    Executor processArguments(String[] args)
    {
        if (args.length != 2) return null;

        File outputDir = new File(args[0]);
        File repoDir   = new File(args[1]);

        if (outputDir.isFile())
        {
            System.err.print("Output location is a file: ");
            System.err.println(args[0]);
            return null;
        }

        if (! repoDir.isDirectory())
        {
            System.err.print("Repository is not a directory: ");
            System.err.println(args[1]);
            return null;
        }

        return new Executor(outputDir, repoDir);
    }


    private static class Executor
        implements Command.Executor
    {
        private final FusionRuntime myRuntime;
        private final File myOutputDir;
        private final File myRepoDir;

        private Executor(File outputDir, File repoDir)
        {
            myRuntime   = FusionRuntimeBuilder.standard().build();
            myOutputDir = outputDir;
            myRepoDir   = repoDir;
        }


        @Override
        public void execute()
            throws Exception
        {
            documentDir(myRepoDir, new ModuleDoc(myRuntime, myOutputDir));
        }


        /**
         *
         * @param dir
         * @param doc will accumulate the documentation for the dir
         */
        private void documentDir(File dir, ModuleDoc doc)
            throws IOException, FusionException
        {
            String[] fileNames = dir.list();

            for (String fileName : fileNames)
            {
                if (fileName.equals("private")) continue;

                File testFile = new File(dir, fileName);
                if (testFile.isDirectory())
                {
                    ModuleDoc d = doc.containedModule(fileName);
                    documentDir(testFile, d);
                    d.writeDirDocs();
                }
                else if (fileName.endsWith(".ion"))
                {
                    // We assume that all .ion files are modules.
                    String moduleName =
                        fileName.substring(0, fileName.length() - 4);
                    ModuleDoc d = doc.containedModule(moduleName);
                    d.writeFileDocs();
                }
            }
        }
    }


    //========================================================================


    private static final class ModuleDoc
    {
        private final FusionRuntime myRuntime;
        private final String myModuleName;
        private final String myModulePath;
        private final File myOutputDir;
        private final File myOutputFile;

        private Map<String,ModuleDoc> mySubmodules;


        /** Root of doc tree */
        ModuleDoc(FusionRuntime runtime, File outputDir)
        {
            myRuntime = runtime;
            myModuleName = "";
            myModulePath = "";
            myOutputDir = outputDir;
            myOutputFile = null;
        }

        private ModuleDoc(ModuleDoc parent, String moduleName)
        {
            File parentOutputDir = parent.myOutputDir;
            myRuntime    = parent.myRuntime;
            myModuleName = moduleName;
            myModulePath = parent.myModulePath + "/" + moduleName;
            myOutputDir  = new File(parentOutputDir, moduleName);
            myOutputFile = new File(parentOutputDir, moduleName + ".html");
        }

        ModuleDoc containedModule(String moduleName)
        {
            ModuleDoc doc = new ModuleDoc(this, moduleName);

            if (mySubmodules == null)
            {
                mySubmodules = new HashMap<String,ModuleDoc>();
            }

            assert ! mySubmodules.containsKey(moduleName);
            mySubmodules.put(moduleName, doc);

            return doc;
        }


        private void writeText(String text)
            throws IOException, FusionException
        {
            myOutputFile.getParentFile().mkdirs();

            FileWriter fw = new FileWriter(myOutputFile);
            try
            {
                fw.write(text);
            }
            finally
            {
                fw.close();
            }
        }

        private void writeHtml(String markdown)
            throws IOException, FusionException
        {
            MarkdownProcessor processor = new MarkdownProcessor();
            String html = processor.markdown(markdown);
            writeText(html);
        }

        void writeFileDocs()
            throws IOException, FusionException
        {
            StringBuilder out = new StringBuilder();
            documentModule(out, myRuntime, myModulePath);

            writeHtml(out.toString());
        }

        void writeDirDocs()
            throws IOException, FusionException
        {
            StringBuilder out = new StringBuilder();
            out.append("## Module ");
            out.append(myModulePath);
            out.append("\n\nSubmodules:\n\n");

            String[] names = mySubmodules.keySet().toArray(new String[0]);
            Arrays.sort(names);

            for (String name : names)
            {
                out.append("* [");
                out.append(name);
                out.append("](");
                out.append(myModuleName);
                out.append('/');
                out.append(name);
                out.append(".html)\n");
            }

            writeHtml(out.toString());
        }
    }
}
