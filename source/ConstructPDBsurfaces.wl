(* ::Package:: *)

BeginPackage["ProteinSurfaces`"];
  
  DownloadPDB::usage = "Download usage";
  InstallConstructPDBsurfaces::usage = "Install usage";
  ProtonatePDB::usage = "Protonate usage";
  DrawPDBSAS::usage = "Draw usage";
  ConstructPDBSES::usage = "Construct usage";
  
  Begin["`Private`"];
  
  failStr = "The function failed. The failure occured due to absent file `1` ";
  msmsArchiveLinks = {"Windows"-> "http://mgltools.scripps.edu/downloads/tars/releases/MSMSRELEASE/REL2.6.1/msms_win32_2.6.1.zip",
  "Unix"->"http://mgltools.scripps.edu/downloads/tars/releases/MSMSRELEASE/REL2.6.1/msms_i86_64Linux2_2.6.1.tar.gz",
  "MacOSX"->"http://mgltools.scripps.edu/downloads/tars/releases/MSMSRELEASE/REL2.6.1/msms_MacOSX_2.6.1.tar.gz"};
  msmsExeMask = {"Windows"->"msms.exe","Unix"->"msms.x86_64Linux*","MacOSX"->"msms.MacOSX.*"};
  reduceArchiveLinks = {"Windows"->"http://kinemage.biochem.duke.edu/downloads/software/reduce31/reduce.3.16.111118.winArchive.zip",
  "Unix"->"http://kinemage.biochem.duke.edu/downloads/software/reduce31/reduce.3.23.130521.linuxi386.gz",
  "MacOSX"->"http://kinemage.biochem.duke.edu/downloads/software/reduce31/reduce.3.23.130521.macosx.zip"};
  
  publishToCloud[filename] :=
      CopyFile[FileNameJoin[{NotebookDirectory[],filename}],CloudObject[FileNameJoin[{"Published",filename}],Permissions->"Public"],OverwriteTarget->True];
  
  checkFilePresentQ[filepath_, verbose_:False] :=
      Module[ {found = FileExistsQ[filepath]},
          If[ verbose,
              Print["searching for '", filepath, "' \[Ellipsis] ",If[ !found,
                                                              Style["NOT",Red],
                                                              ""
                                                          ], " found"];
          ];
          found
      ];
  
  getFilenameSafe[path_, ptrn_] :=
      Module[ {names = FileNames[ptrn,path]},
          If[ Length[names]>0,
              names[[1]],
              FileNameJoin[{path, StringJoin[RandomChoice[Alphabet[],50]]}]
          ]
      ];
  
  getLinkFilename[path_, link_] :=
      FileNameJoin[{path, FileBaseName[FileNameTake[link]]}];
  
  loadNew[path_, link_, verbose_:False] :=
      Module[ {archiveFile,unpackedFiles},
          If[ verbose,
              Print["loading from '", link,"'"];
              Print["loading to: ", path];
          ];
          archiveFile = URLDownload[link, FileNameJoin[{path, FileNameTake[link]}],CreateIntermediateDirectories->True];
          If[ verbose,
              Print[archiveFile, " loaded"];
          ];
          unpackedFiles = ExtractArchive[archiveFile,path,OverwriteTarget->True,CreateIntermediateDirectories->True];
          If[ verbose,
              Print["unpacked: '", unpackedFiles,"'"]
          ];
          DeleteFile[archiveFile];
          unpackedFiles
      ];
  
  loadIfAbsent[path_, link_, verbose_:False] :=
      Module[ {filepath},
          filepath = getLinkFilename[path, link];
          If[ !checkFilePresentQ[filepath, verbose],
              loadNew[DirectoryName[filepath], link, verbose];
          ];
          filepath
      ];
  
  fileIsExecutableQ[cmd_] :=
      (Quiet[Check[RunProcess[cmd,"ExitCode"],"err"]]!="err");
  fileIsTheRightExecutableQ[cmd_, testStr_:""] :=
      Module[ {localErrorFlag = "err",stdStr,errStr},
          If[ fileIsExecutableQ[cmd],
              False,
              If[ StringStartsQ[RunProcess[cmd,"StandardError"], testStr],
                  True,
                  StringStartsQ[RunProcess[cmd,"StandardOutput"], testStr]
              ]
          ]
      ];
  
  absoluteDirPath[dir_] :=
      FileNameJoin[{ParentDirectory[dir,1],FileNameTake[dir]}];
      
  isSubdir[rootDir_, subDir_] :=
      StringStartsQ[absoluteDirPath[subDir], absoluteDirPath[rootDir]];
  
  replaceStringInFiles[filenames_, oldStr_, newStr_] :=
      Module[ {nFiles = Length[filenames],filename},
          Do[filename = filenames[[i]];
             Export[filename,StringReplace[Import[filename,"Text"],oldStr->newStr],"Text"],{i,1,nFiles}];
      ];

  safeDirDelete[dir_,deleteContents_:True] :=
      If[ DirectoryQ[dir],
          If[ isSubdir[dir, Directory[]],
              SetDirectory[ParentDirectory[dir,100]]
          ];
          DeleteDirectory[dir,DeleteContents->deleteContents];
      ];
      
  
  checkFileThrow[filePath_] :=
      If[ !FileExistsQ[filePath],
          Throw[$Failed,fileNotFoundTag[filePath]];
      ];
  
  writeAFile[str_, filepathArg_:""] :=
      Module[ {filepath,tmpfile = (filepathArg=="")},
          filepath = If[ tmpfile,
                         CreateFile[OverwriteTarget->True],
                         filepathArg
                     ];
          WriteString[filepath,str];
          Close[filepath];
          If[ tmpfile,
              filepath,
              Nothing
          ]
      ];
  
  renewDir[pathArg_] :=
      Module[ {path = pathArg},
          safeDirDelete[path];
          CreateDirectory[path];
      ];
      
  parseProbeR[probeRarg_, radiiType_] :=
      Module[ {probeR = probeRarg},
          If[ NumberQ[probeR],
              probeR = Quantity[probeR, "Angstroms"],
              If[ QuantityMagnitude[probeR] < 0, (*let the system choose*)
                  probeR = UnitConvert[Min[ElementData["O", radiiType<>"Radius"],
                  ElementData["H", radiiType<>"Radius"]],"Angstroms"];
              ];
          ];
          probeR
      ];
      
  constructPDBSAS[coords_, radii_, probeR_, colors_] :=
      Table[{colors[[i]], Sphere[QuantityMagnitude[coords[[i]]], QuantityMagnitude[radii[[i]]+probeR]]},{i, 1, Length[radii]}];
      
  Options[readXYZRdatabase] = {"explicitH"->False, "Verbose"->False};
  readXYZRdatabase[filepath_, OptionsPattern[]] :=
      Module[ {datafile, radiiData, radiiTable, residData,i,radii,
      verbose = OptionValue["Verbose"],
      explicitH = OptionValue["explicitH"]},
          If[ verbose,
              Print["database: ", filepath];
          ];
          datafile = Map[If[ StringPart[#,1]!="#",
                             StringSplit[StringDelete[#,"#"~~___]],
                             Nothing
                         ]&,ReadList[filepath, String]];
          radiiData = Map[
          Module[ {words = #},
              If[ words[[1]]=="radius",
                  If[ Length[words]==5,
                      words[[2;;]],
                      Append[words[[2;;]], words[[4]] ]
                  ],
                  Nothing
              ]
          ]&
          ,datafile];
          radiiData = Map[Read[StringToStream[#],Number]&,radiiData,{2}];
          If[ verbose,
              Print["radii data: ",radiiData];
          ];
          For[i = 1, i<Length[radiiData],i++,
          (*this is the place to implemet -h flag from the original script*)
          radii[radiiData[[i]][[1]]] = radiiData[[i]][[If[ explicitH,
                                                           3,
                                                           4
                                                       ]]];
          If[ verbose,
              Print["r[",radiiData[[i]][[1]],"] = ", radii[radiiData[[i]][[1]]] ]
          ];
          ];
          residData = Map[
          Module[ {words = #},
              If[ words[[1]]!="radius",
                  words,
                  Nothing
              ]
          ]&
          ,datafile];
          residData = 
          Map[Module[ {residPatt = #[[1]], atomPatt = #[[2]]},
                  If[ residPatt=="*",
                      residPatt = ".*"
                  ];
                  residPatt = "^"<>residPatt<>"$";
                  atomPatt = "^"<>atomPatt<>"$";
                  {residPatt, atomPatt, Read[StringToStream[#[[3]]],Number]}
              ]&
          ,
          residData];
          If[ verbose,
              Print["residue data: ",residData];
          ];
          {radii, residData}
      ]

  Options[pdb2xyzr] = {"RDefault"->0.01, "Verbose"->False};
  pdb2xyzr[pdb_, radii_, residData_, OptionsPattern[]] :=
      Module[ {nPatterns = Length[residData], 
      verbose = OptionValue["Verbose"],
      rDefault = OptionValue["RDefault"]},
          Map[
          If[ MemberQ[{"ATOM", "atom", "HETATM", "hetatm"}, TextWords[#,1][[1]]],
              Module[ {line = #,x,y,z,r,resName,atomName, resNum,pattI, beginAtomName},
                  atomName = StringTake[line, {13,16}];
                  resName = StringDelete[StringTake[line, {18,20}]," "];
                  resNum = StringDelete[StringTake[line, {23,26}], " "];
                  x = Read[StringToStream[StringTake[line, {31,38}]],Number];
                  y = Read[StringToStream[StringTake[line, {39,46}]],Number];
                  z = Read[StringToStream[StringTake[line, {47,54}]],Number];
                  beginAtomName = StringTake[atomName,{1,2}];
                  If[ StringMatchQ[beginAtomName,RegularExpression["[ 0-9][HhDd]"]] || StringMatchQ[beginAtomName,RegularExpression["[Hh][^Gg]"]],
                      atomName = "H";
                  ];
                  atomName = StringDelete[atomName, " "];
                  If[ verbose,
                      Print[resNum, ": ", atomName, " ", resName," (",x, ";",y, ";",z, ")"]
                  ];
                  For[pattI = 1, pattI<=nPatterns,pattI++,
                  If[ StringMatchQ[atomName, RegularExpression[residData[[pattI]][[2]] ] ]&&
                  StringMatchQ[resName, RegularExpression[residData[[pattI]][[1]] ] ],
                      Break[];
                  ];
                ];
                  If[ pattI == nPatterns+1,
                      Print["pdb2xyzr ERROR:\nprotein: ",ImportString[pdb,{"PDB","PDBID"}],
                      "\nline: ", line,
                      "\nresidue number: ", resNum,
                      "\nread PATTERN: ", resName, "; ", atomName, " was NOT FOUND",
                      "\nusing r = ", rDefault];
                      r = rDefault;,
                      r = radii[residData[[pattI]][[3]]];
                  ];
                  {x,y,z,r}
              ],
              Nothing
          ]&
          ,StringSplit[pdb,"\n"]]
      ];      
    
  Options[InstallConstructPDBsurfaces] = {"RootPath"->FileNameJoin[{$WolframDocumentsDirectory,"PDBsys"}],"DoMSMS"->True,"DoReduce"->True,"Verbose"->False,"forceReinstall"->False,"os"->$OperatingSystem};
  InstallConstructPDBsurfaces[OptionsPattern[]] :=
      Module[ {reducePath,reduceExePath,reduceDatabasePath, reduceExeLink, reduceDatabaseLink,
      msmsPath,msmsExePath,pdb2xyzrExePath,pdb2xyzrDatabasePath,unpackedArch,paths,
      rootPath = OptionValue["RootPath"],
      doMSMS = OptionValue["DoMSMS"],
      doReduce = OptionValue["DoReduce"],
      verbose = OptionValue["Verbose"],
      os = OptionValue["os"]},
          If[ OptionValue["forceReinstall"],
              safeDirDelete[rootPath];
              Return[InstallConstructPDBsurfaces["forceReinstall"->False]];
          ];
          If[ !DirectoryQ[rootPath],
              CreateDirectory[rootPath]
          ];
          If[ doMSMS,
              msmsPath = FileNameJoin[{rootPath, "MSMS"}];
              msmsExePath = getFilenameSafe[msmsPath, os/.msmsExeMask];
              pdb2xyzrExePath = FileNameJoin[{msmsPath, "pdb_to_xyzr"}];
              pdb2xyzrDatabasePath = FileNameJoin[{msmsPath, "atmtypenumbers"}];
              If[ !(checkFilePresentQ[msmsExePath, verbose]&&
              checkFilePresentQ[pdb2xyzrExePath, verbose]&&
              checkFilePresentQ[pdb2xyzrDatabasePath, verbose]),
                  If[ os=="Windows",
                      unpackedArch = loadNew[rootPath, os/.msmsArchiveLinks,verbose];
                      safeDirDelete[msmsPath];
                      RenameDirectory[DirectoryName[Select[unpackedArch, StringMatchQ[#, __~~".pdb"]&][[1]]], msmsPath];,
                      unpackedArch = loadNew[msmsPath, os/.msmsArchiveLinks,verbose];
                  ];
              ];
              msmsExePath = getFilenameSafe[msmsPath, os/.msmsExeMask];
              replaceStringInFiles[{pdb2xyzrExePath}, "nawk", "awk"];
              If[ os!="Windows",
                  If[ !fileIsExecutableQ[msmsExePath],
                      RunProcess[{"chmod", "u+x", msmsExePath}];
                  ];
                  If[ !fileIsExecutableQ[{pdb2xyzrExePath, "1"}],
                      RunProcess[{"chmod", "u+x", msmsExePath}];
                  ];
              ];,
              {msmsExePath, pdb2xyzrExePath, pdb2xyzrDatabasePath} = {"","",""};
          ];
          If[ doReduce,
              reducePath = FileNameJoin[{rootPath, "Reduce"}];
              If[ os=="Windows",
                  reduceExePath = FileNameJoin[{reducePath, "reduce.exe"}];
                  reduceDatabasePath = FileNameJoin[{reducePath, "reduce_wwPDB_het_dict.txt"}];
                  If[ !(checkFilePresentQ[reduceExePath, verbose] && checkFilePresentQ[reduceDatabasePath, verbose]),
                      safeDirDelete[reducePath];
                      If[ FileExistsQ[reducePath],
                          DeleteFile[reducePath]
                      ];
                      unpackedArch = loadNew[rootPath, os/.reduceArchiveLinks, verbose];
                      RenameDirectory[unpackedArch[[1]], reducePath];
                  ];,
                  reduceExePath = loadIfAbsent[reducePath,os/.reduceArchiveLinks, verbose];
                  reduceDatabasePath = loadIfAbsent[reducePath,"http://kinemage.biochem.duke.edu/downloads/software/reduce31/reduce_wwPDB_het_dict.txt.zip", verbose];
                  If[ !fileIsExecutableQ[reduceExePath],
                      RunProcess[{"chmod", "u+x", reduceExePath}];
                  ];
              ];,
              {reduceExePath, reduceDatabasePath} = {"",""};
          ];
          paths = {msmsExePath, pdb2xyzrExePath, pdb2xyzrDatabasePath,reduceExePath, reduceDatabasePath};
          If[ verbose,
              Print[paths]
          ];
          paths
      ];
  
  DownloadPDB[structID_] :=
      Import["http://www.rcsb.org/pdb/download/downloadFile.do?fileFormat=pdb&compression=NO&structureId="<>structID,"String"];
  
  ProtonatePDB[pdbStr_, reduceOptions_:reduceDefaultArgs] :=
      Module[ {tempFilepath,pdbProtonated,cmd},
          checkFileThrow[reduceExePath];
          tempFilepath = writeAFile[pdbStr];
          cmd = "!"<>reduceExePath<>" "<>TextString[reduceOptions,ListFormat->{""," ",""}]<>" "<>tempFilepath;
          pdbProtonated = StringJoin[ReadList[cmd,Character]];
          DeleteFile[tempFilepath];
          pdbProtonated
      ];
  (*
  Module[{},
  checkFileQ[reduceExePath];
  Print[Flatten[{reduceExePath, reduceOptions, "-"}]];
  RunProcess[Flatten[{reduceExePath, reduceOptions, "-"}], "StandardOutput", pdbStr]
  ];*)
  
  Options[DrawPDBSAS] = {"ProbeR"->Quantity[-1,"Angstroms"], "RadiiType"->"VanDerWaals","Verbose"->True};
  (*"Atomic", "Covalent", "VanDerWaals"*)
  DrawPDBSAS[pdbStr_,OptionsPattern[]] :=
      Module[ {elements,radiiTable,presentElements,colorTable, radiiType, probeR, coords,verbose},
          verbose = OptionValue["Verbose"];
          radiiType = OptionValue["RadiiType"];
          probeR = parseProbeR[OptionValue["ProbeR"], radiiType];
          elements = ImportString[pdbStr,{"PDB",{"VertexTypes"}}];
          presentElements = DeleteDuplicates[elements];          
          colorTable = Map[(#->ColorData["Atoms"][#])&, presentElements];
          radiiTable = Map[(#->UnitConvert[ElementData[#, radiiType<>"Radius"],"Angstroms"])&,presentElements];
          coords = Map[Quantity[#,"Angstroms"]&,ImportString[pdbStr,{"PDB","VertexCoordinates"}]/100];
          If[verbose,Print["radii type: ", radiiType,
          "\nprobe radius = ", probeR,
          "\n", radiiTable,
          "\n", colorTable]
          ];
          sas = constructPDBSAS[coords, elements/.radiiTable, probeR, elements/.colorTable];
          Return[Graphics3D[sas,Boxed->False]];
      ];
      
  Options[ConstructPDBSES] = {"TriangDensity"->5.0, "ProbeR"->Quantity[1.5,"Angstroms"], "Verbose"->False,"RootPath"->$TemporaryDirectory};     
  ConstructPDBSES[pdbStr_,OptionsPattern[]] :=
      Module[ {rawPDBfilepath,rawPDBfilename,pdbFilename, pdbFilepath, xyzrFilename,xyzrFilepath,vertFilename,
          faceFilename,proteinPath,vertices,triangleIndices,nTriangles,mesh,degeneratePolygonInd,
          meshInd,msmsDir,pdb2xyzrExePath,pdb2xyzrDatabasePath,cmd,pdbName,radiiData,residueData,
      triangDensity = OptionValue["TriangDensity"],
      probeR = QuantityMagnitude[UnitConvert[parseProbeR[OptionValue["ProbeR"], "VanDerWaals"],"Angstroms"]],
      verbose = OptionValue["Verbose"],
      rootPath = OptionValue["RootPath"]
      },
          pdbName = ToLowerCase[ImportString[pdbStr,{"PDB",{"PDBID"}}]];
          If[ verbose,
              Print["PDB ID: ", pdbName,
              "\nProbeR = ", probeR,
              "\nTriangDensity = ", triangDensity,
              "\nRootPath: '", rootPath, "'"];
          ];
          
        (*path routines*)
          msmsDir = DirectoryName[msmsExePath];
          pdb2xyzrExePath = FileNameJoin[{msmsDir, "pdb_to_xyzr"}];
          pdb2xyzrDatabasePath = FileNameJoin[{msmsDir, "atmtypenumbers"}];
          checkFile[pdb2xyzrExePath];
          checkFile[pdb2xyzrDatabasePath];
          checkFile[msmsExePath];
          pdbFilename = pdbName<>".pdb";
          xyzrFilename = pdbName<>".xyzr";
          vertFilename = pdbName<>".vert";
          faceFilename = pdbName<>".face";
          proteinPath = FileNameJoin[{rootPath, pdbName}];
          pdbFilepath = FileNameJoin[{proteinPath, pdbFilename}];
          xyzrFilepath = FileNameJoin[{proteinPath, xyzrFilename}];
          If[ verbose,
              Print["input pdb: ", pdbFilepath,
              "\nxyzr file: ", xyzrFilepath,
              "\nvertices file: ", FileNameJoin[{proteinPath, vertFilename}],
              "\ntriangulation file: ", FileNameJoin[{proteinPath, faceFilename}]
              ];
          ];
            
            (*reduce & msms utilization*)
          renewDir[proteinPath];
          SetDirectory[msmsDir];
          writeAFile[pdbStr, pdbFilepath];
          If[ verbose,
              Print["runnig pdb2xyzr \[Ellipsis]"]
          ];
          If[ $OperatingSystem=="Windows",
              {radiiData, residueData} = readXYZRdatabase[pdb2xyzrDatabasePath];
              writeAFile[StringJoin[Map[(ToString[#[[1]]]<>" "<>ToString[#[[2]]]<>" "<>ToString[#[[3]]]<>" "<>ToString[#[[4]]]<>"\n")&, 
              pdb2xyzr[pdbStr, radiiData, residueData]]],xyzrFilepath];,
              writeAFile[RunProcess[{pdb2xyzrExePath,"-h", pdbFilepath}, "StandardOutput"], xyzrFilepath]
          ];
          If[ verbose,
              Print["done\nrunnig MSMS \[Ellipsis]"]
          ];
          RunProcess[{msmsExePath, "-no_header", "-probe_radius",ToString[probeR],"-density",ToString[triangDensity],"-if",xyzrFilepath,"-of",FileNameJoin[{proteinPath, pdbName}]}, "StandardOutput"];
          If[ verbose,
              Print["done"]
          ];
          SetDirectory[proteinPath];
          vertices = Map[Quantity[#[[1;;3]],"Angstroms"]&,ReadList[vertFilename, {Real, Real, Real, Real, Real, Real, Number, Number, Number}]];
          triangleIndices = Map[#[[1;;3]]&,ReadList[faceFilename, {Number, Number, Number, Number, Number}]];
          nTriangles = Length[triangleIndices];
            (*duplicate vertices handling*)
          mesh = Table[ Module[ {trgPoints},
                            trgPoints = {vertices[[ triangleIndices[[i]][[1]] ]], vertices[[ triangleIndices[[i]][[2]] ]], vertices[[ triangleIndices[[i]][[3]] ]]};
                            If[ DuplicateFreeQ[trgPoints],
                                Triangle[trgPoints],
                                Null
                            ]
                        ],
          {i,1,nTriangles}];
          If[ verbose,
              Print["found ", Length[vertices], " vertices, ", nTriangles, " triangles"]
          ];
          degeneratePolygonInd = Position[mesh, Null];
          mesh = Delete[mesh, degeneratePolygonInd];
          meshInd = Table[If[ !MemberQ[Flatten[degeneratePolygonInd],i],
                              Triangle[triangleIndices[[i]]],
                              Nothing
                          ],{i,1,nTriangles}];
            
            (*print some stats*)
          If[ verbose,
              Print["Duplicate vertices: ", If[ !DuplicateFreeQ[vertices],
                                                Part[Select[Tally@vertices,Part[#,2]>1&],All,1],
                                                "None"
                                            ]];
              Print["Degenerate polygon indices: ", Flatten[degeneratePolygonInd]];
              Print["Degenerate polygons: ", triangleIndices[[Flatten[degeneratePolygonInd]]]];
          ];
          If[ rootPath==$TemporaryDirectory,
              safeDirDelete[proteinPath];
              If[ verbose,
                  Print["temporary directory '",proteinPath,"' deleted"]
              ];
          ];
          Return[{mesh, vertices, meshInd}];
      ];
                      
  {msmsExePath, pdb2xyzrExePath, pdb2xyzrDatabasePath, reduceExePath, reduceDatabasePath} = InstallConstructPDBsurfaces[];
  reduceDefaultArgs = {"-build","-DB","\""<>reduceDatabasePath<>"\""};        
    
  End[ ];
  
  EndPackage[ ];


(*
Quiet[Remove["Global`*"]];

<<"/home/ypolyach/!wolfram/WSS2020project/source/pckg.wl";

(*the protein to analyse*)
pdbName = "1tf6";
pdbName = "1aki";

{mesh, vertices, meshInd} = ConstructSESmesh[DownloadPDB[pdbName]];
MeshRegion[QuantityMagnitude[vertices], meshInd,PlotTheme\[Rule]"LargeMesh"]

{mesh, vertices, meshInd} = ConstructSESmesh[ProtonatePDB[DownloadPDB[pdbName]],"RootPath"\[Rule]$WolframDocumentsDirectory];
MeshRegion[QuantityMagnitude[vertices], meshInd,PlotTheme\[Rule]"LargeMesh"]
*)


(*
Quiet[Remove["Global`*"]];

<< "/home/ypolyach/!wolfram/WSS2020project/source/pckg.wl";

pdbName = "1tf6";
pdbName = "1aki";
toProtonate = True;

pdb = DownloadPDB[pdbName];
If[toProtonate, pdb = ProtonatePDB[pdb]];
DrawProteinSAS[pdb]
*)
