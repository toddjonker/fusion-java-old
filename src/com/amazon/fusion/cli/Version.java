// Copyright (c) 2005-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.FusionException;
import com.amazon.fusion.util.FusionJarInfo;
import com.amazon.ion.IonException;
import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
import com.amazon.ion.system.IonTextWriterBuilder;
import com.amazon.ion.util.JarInfo;
import java.io.IOException;

class Version
    extends Command
{
    private static final String HELP_ONE_LINER =
        "Writes version information about this program.";

    private static final String HELP_USAGE =
        "version";

    private static final String HELP_BODY =
        "Writes this program's version and build information to standard output, in Ion\n" +
        "format.";


    //=========================================================================
    // Constructors

    Version()
    {
        super("version");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    //=========================================================================


    @Override
    Executor makeExecutor(String[] arguments)
    {
        return new Executor(arguments);
    }


    private static class Executor
        implements Command.Executor
    {
        private Executor(String[] arguments)
        {
        }


        @Override
        public int execute()
            throws IOException
        {
            IonTextWriterBuilder b = IonTextWriterBuilder.pretty();
            b.setCharset(IonTextWriterBuilder.ASCII);

            JarInfo ionInfo = null;
            FusionJarInfo fusionInfo = null;
            try
            {
                ionInfo = new JarInfo();
                fusionInfo = new FusionJarInfo();
            }
            catch (IonException | FusionException e) { }


            IonWriter w = b.build((Appendable)System.out);
            w.stepIn(IonType.STRUCT);
            {
                if (fusionInfo != null)
                {
                    w.setFieldName("fusion_version");
                    w.stepIn(IonType.STRUCT);
                    {
                        w.setFieldName("release_label");
                        w.writeString(fusionInfo.getReleaseLabel());

                        w.setFieldName("brazil_major_version");
                        w.writeString(fusionInfo.getBrazilMajorVersion());

                        w.setFieldName("brazil_package_version");
                        w.writeString(fusionInfo.getBrazilPackageVersion());

                        w.setFieldName("build_time");
                        w.writeTimestamp(fusionInfo.getBuildTime());
                    }
                    w.stepOut();
                }

                if (ionInfo != null)
                {
                    w.setFieldName("ion_version");
                    w.stepIn(IonType.STRUCT);
                    {
                        w.setFieldName("project_version");
                        w.writeString(ionInfo.getProjectVersion());

                        w.setFieldName("build_time");
                        w.writeTimestamp(ionInfo.getBuildTime());
                    }
                    w.stepOut();
                }
            }
            w.stepOut();
            w.finish();
            System.out.println();

            return 0;
        }
    }
}
