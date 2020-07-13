(* ::Package:: *)

BeginPackage["ProteinSurfaces`"];
  
  DownloadPDB::usage = "Download usage";
  InstallMyPckg::usage = "Install usage";
  ProtonatePDB::usage = "Protonate usage";
  DrawProteinSAS::usage = "Draw usage";
  ConstructSESmesh::usage = "Construct usage";
  
  Begin["`Private`"];
  
  checkFilePresentQ[filepath_, verbose_:False]:=Module[{found = FileExistsQ[filepath]},
  If[verbose,Print["searching for '", filepath, "' ... ",If[!found,Style["NOT",Red],""], " found"]];
  found];
  
  getFilenameSafe[path_, ptrn_]:=Module[{names = FileNames[ptrn,path]},
  If[Length[names]>0,names[[1]],FileNameJoin[{path, StringJoin[RandomChoice[Alphabet[],50]]}]]];
  
  getLinkFilename[path_, link_] := FileNameJoin[{path, FileBaseName[FileNameTake[link]]}];
  
  loadNew[path_, link_, verbose_:False]:=
  Module[{archiveFile,unpackedFiles},
  If[verbose,Print["loading from '", link,"'"];];
  archiveFile = URLDownload[link, FileNameJoin[{path, FileNameTake[link]}],CreateIntermediateDirectories->True];
  If[verbose,Print[archiveFile, " loaded"];];
  unpackedFiles = ExtractArchive[archiveFile,path,OverwriteTarget->True,CreateIntermediateDirectories->True];
  If[verbose, Print["unpacked: '", unpackedFiles,"'"]];
  DeleteFile[archiveFile];
  unpackedFiles
  ];
  
  loadIfAbsent[path_, link_, verbose_:False]:=Module[{filepath},
  filepath = getLinkFilename[path, link];
  If[!checkFilePresentQ[filepath, verbose],
  loadNew[DirectoryName[filepath], link, verbose];
  ];
  filepath
  ];
  
  (*fileIsExecutableQ[cmd_]:= (Quiet[Check[RunProcess[cmd,"ExitCode"],"err"]]\[NotEqual]"err");*)
  fileIsExecutableQ[cmd_]:= False;
  fileIsTheRightExecutableQ[cmd_, testStr_:""]:=
  Module[{localErrorFlag="err",stdStr,errStr},
  If[fileIsExecutableQ[cmd],
  False,
  If[StringStartsQ[RunProcess[cmd,"StandardError"], testStr],
  True,
  StringStartsQ[RunProcess[cmd,"StandardOutput"], testStr]
  ]
  ]
  ];
  
  InstallMyPckg[OptionsPattern[]]:=
  Module[{reducePath,reduceExePath,reduceDatabasePath, reduceExeLink, reduceDatabaseLink,
  msmsPath,msmsExePath,pdb2xyzrExePath,pdb2xyzrDatabasePath,unpackedArch,paths,
  rootPath = OptionValue["rootPath"],
  doMSMS = OptionValue["doMSMS"],
  doReduce = OptionValue["doReduce"],
  verbose = OptionValue["verbose"],
  os = OptionValue["os"]},
  If[OptionValue["forceReinstall"],
  DeleteDirectory[rootPath,DeleteContents->True];
  Return[InstallMyPckg["forceReinstall"->False]];];
  
  If[!DirectoryQ[rootPath],
  CreateDirectory[rootPath,CreateIntermediateDirectories->True]];
  
  If[doMSMS,
  msmsPath = FileNameJoin[{rootPath, "MSMS"}];
  msmsExePath = getFilenameSafe[msmsPath, os/.msmsExeMask];
  pdb2xyzrExePath = FileNameJoin[{msmsPath, "pdb_to_xyzr"}];
  pdb2xyzrDatabasePath = FileNameJoin[{msmsPath, "atmtypenumbers"}];
  If[!(checkFilePresentQ[msmsExePath, verbose]&&
  checkFilePresentQ[pdb2xyzrExePath, verbose]&&
  checkFilePresentQ[pdb2xyzrDatabasePath, verbose]),
  loadNew[msmsPath, os/.msmsArchiveLinks,verbose];
  ];
  msmsExePath = getFilenameSafe[msmsPath, os/.msmsExeMask];
  If[os!="Windows",
  If[!fileIsExecutableQ[msmsExePath],
  RunProcess[{"chmod", "u+x", msmsExePath}];];
  If[!fileIsExecutableQ[{pdb2xyzrExePath, "1"}],
  RunProcess[{"chmod", "u+x", msmsExePath}];];
  ];
  ,
  {msmsExePath, pdb2xyzrExePath, pdb2xyzrDatabasePath} = {"","",""};
  ];
  
  If[doReduce,
  reducePath = FileNameJoin[{rootPath, "Reduce"}];
  If[os=="Windows",
  reduceExePath = FileNameJoin[{reducePath, "reduce.exe"}];
  reduceDatabasePath = FileNameJoin[{reducePath, "reduce_wwPDB_het_dict.txt"}];
  If[!(checkFilePresentQ[reduceExePath, verbose] && checkFilePresentQ[reduceDatabasePath, verbose]),
  If[DirectoryQ[reducePath],DeleteDirectory[reducePath,DeleteContents->True]];
  If[FileExistsQ[reducePath],DeleteFile[reducePath]];
  unpackedArch = loadNew[rootPath, os/.reduceArchiveLinks, verbose];
  RenameDirectory[unpackedArch[[1]], reducePath];
  ];
  ,
  reduceExePath = loadIfAbsent[reducePath,os/.reduceArchiveLinks, verbose];
  reduceDatabasePath = loadIfAbsent[reducePath,"http://kinemage.biochem.duke.edu/downloads/software/reduce31/reduce_wwPDB_het_dict.txt.zip", verbose];
  If[!fileIsExecutableQ[reduceExePath],
  RunProcess[{"chmod", "u+x", reduceExePath}];];
  ];
  ,
  {reduceExePath, reduceDatabasePath} = {"",""};
  ];
  
  paths = {msmsExePath, pdb2xyzrExePath, pdb2xyzrDatabasePath,reduceExePath, reduceDatabasePath};
  If[verbose,Print[paths]];
  paths
  ];
  
  checkFileThrow[filePath_]:=
  Module[{},
  If[!FileExistsQ[filePath], 
  Throw[$Failed,fileNotFoundTag[filePath]];
  ];
  ];
  
  writeAFile[str_, filepathArg_:""]:=
  Module[{filepath,tmpfile = (filepathArg=="")},
  filepath=If[tmpfile,CreateFile[OverwriteTarget->True],filepathArg];
  WriteString[filepath,str];
  Close[filepath];
  If[tmpfile, filepath, Nothing]
  ];
  
  renewDir[path_]:=
  Module[{},
  If[DirectoryQ[path],
  DeleteDirectory[path,DeleteContents->True];
  ];
  CreateDirectory[path];
  ];
  
  DownloadPDB[structID_]:=Import["http://www.rcsb.org/pdb/download/downloadFile.do?fileFormat=pdb&compression=NO&structureId="<>structID,"String"];
  
  ProtonatePDB[pdbStr_, reduceOptions_:reduceDefaultArgs]:=
  Module[{tempFilepath,pdbProtonated,cmd},
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
  
  constructProteinSAS[coords_, radii_, probeR_, colors_]:=Table[{colors[[i]], Sphere[QuantityMagnitude[coords[[i]]], QuantityMagnitude[radii[[i]]+probeR]]},{i, 1, Length[radii]}];
  
  ConstructSESmesh[pdbStr_,OptionsPattern[]]:=
  Module[
  {rawPDBfilepath,rawPDBfilename,pdbFilename, pdbFilepath, xyzrFilename,xyzrFilepath,vertFilename,faceFilename,proteinPath,vertices,nVertices,triangleIndices,nTriangles,mesh,degeneratePolygonInd,meshInd,msmsDir,pdb2xyzrExePath,pdb2xyzrDatabasePath,cmd,pdbName,
  triangDensity=OptionValue["triangDensity"],
  probeR = OptionValue["probeR"],
  toPrint = OptionValue["toPrint"],
  rootPath = OptionValue["rootPath"]
  },
  
  (*path routines*)
  msmsDir = DirectoryName[msmsExePath];
  pdb2xyzrExePath = FileNameJoin[{msmsDir, "pdb_to_xyzr"}];
  pdb2xyzrDatabasePath = FileNameJoin[{msmsDir, "atmtypenumbers"}];
  checkFile[pdb2xyzrExePath];
  checkFile[pdb2xyzrDatabasePath];
  checkFile[msmsExePath];
  pdbName = ToLowerCase[ImportString[pdbStr,{"PDB",{"PDBID"}}]];
  pdbFilename = pdbName<>".pdb";
  xyzrFilename = pdbName<>".xyzr";
  vertFilename = pdbName<>".vert";
  faceFilename = pdbName<>".face";
  proteinPath = FileNameJoin[{rootPath, pdbName}];
  pdbFilepath = FileNameJoin[{proteinPath, pdbFilename}];
  xyzrFilepath = FileNameJoin[{proteinPath, xyzrFilename}];
  renewDir[proteinPath];
  
  (*reduce & msms utilization*)
  SetDirectory[msmsDir];
  writeAFile[pdbStr, pdbFilepath];
  writeAFile[RunProcess[{pdb2xyzrExePath, pdbFilepath}, "StandardOutput"], xyzrFilepath];
  RunProcess[{msmsExePath, "-no_header", "-probe_radius",ToString[probeR],"-density",ToString[triangDensity],"-if",xyzrFilepath,"-of",FileNameJoin[{proteinPath, pdbName}]}, "StandardOutput"];
  SetDirectory[proteinPath];
  vertices = Map[#[[1;;3]]&,ReadList[vertFilename, {Real, Real, Real, Real, Real, Real, Number, Number, Number}]];
  nVertices = Length[vertices];
  triangleIndices = Map[#[[1;;3]]&,ReadList[faceFilename, {Number, Number, Number, Number, Number}]];
  nTriangles = Length[triangleIndices];
  (*duplicate vertices handling*)
  mesh = Table[
  Module[{trgPoints},
  trgPoints = {vertices[[ triangleIndices[[i]][[1]] ]], vertices[[ triangleIndices[[i]][[2]] ]], vertices[[ triangleIndices[[i]][[3]] ]]};
  If[DuplicateFreeQ[trgPoints],Triangle[trgPoints],Null]
  ],
  {i,1,nTriangles}];
  degeneratePolygonInd = Position[mesh, Null];
  mesh = Delete[mesh, degeneratePolygonInd];
  meshInd = Table[If[!MemberQ[Flatten[degeneratePolygonInd],i],Triangle[triangleIndices[[i]]], Nothing],{i,1,nTriangles}];
  
  (*print some stats*)
  If[toPrint,
  Print["Duplicate vertices: ", If[!DuplicateFreeQ[vertices],Part[Select[Tally@vertices,Part[#,2]>1&],All,1],Null]];Print["Degenerate polygon indices: ", Flatten[degeneratePolygonInd]];
  Print["Degenerate polygons: ", triangleIndices[[Flatten[degeneratePolygonInd]]]];
  ];
  If[rootPath==$TemporaryDirectory, DeleteDirectory[proteinPath,DeleteContents->True];];
  
  Return[{mesh, vertices, meshInd}];
  ];
  
  DrawProteinSAS[pdbStr_,OptionsPattern[]]:=
  Module[{elements,radiiTable,presentElements,colorTable, radiiType, probeR, coords},
  radiiType = OptionValue["radiiType"];
  probeR = OptionValue["probeR"]; 
  elements = ImportString[pdbStr,{"PDB",{"VertexTypes"}}];
  presentElements = DeleteDuplicates[elements];
  colorTable = Map[(#->ColorData["Atoms"][#])&, presentElements];
  radiiTable = Map[(#->UnitConvert[ElementData[#, radiiType],"Angstroms"])&,presentElements];
  coords = Map[Quantity[#,"Angstroms"]&,ImportString[pdbStr,{"PDB","VertexCoordinates"}]/100];
  Print[radiiType, "; probe radius = ", probeR];
  Print[radiiTable];
  Print[colorTable];
  sas = constructProteinSAS[coords, elements/.radiiTable, probeR, elements/.colorTable];
  Return[Graphics3D[sas]];
  ];
  
  
  (*should be in Initialize[]*)
  failStr="The function failed. The failure occured due to absent file `1` ";
  msmsArchiveLinks = {"Windows"-> "http://mgltools.scripps.edu/downloads/tars/releases/MSMSRELEASE/REL2.6.1/msms_win32_2.6.1.zip",
  "Unix"->"http://mgltools.scripps.edu/downloads/tars/releases/MSMSRELEASE/REL2.6.1/msms_i86_64Linux2_2.6.1.tar.gz",
  "MacOSX"->"http://mgltools.scripps.edu/downloads/tars/releases/MSMSRELEASE/REL2.6.1/msms_MacOSX_2.6.1.tar.gz"};
  msmsExeMask = {"Windows"->"msms.exe","Unix"->"msms.x86_64Linux*","MacOSX"->"msms.MacOSX.*"};
  reduceArchiveLinks = {"Windows"->"http://kinemage.biochem.duke.edu/downloads/software/reduce31/reduce.3.16.111118.winArchive.zip",
  "Unix"->"http://kinemage.biochem.duke.edu/downloads/software/reduce31/reduce.3.23.130521.linuxi386.gz",
  "MacOSX"->"http://kinemage.biochem.duke.edu/downloads/software/reduce31/reduce.3.23.130521.macosx.zip"};
  Options[InstallMyPckg] = {"rootPath"->FileNameJoin[{$WolframDocumentsDirectory,"PDBsys"}],"doMSMS"->True,"doReduce"->True,"verbose"->False,"forceReinstall"->False,"os"->$OperatingSystem};
  Options[DrawProteinSAS] = {"probeR"->Quantity[1.5,"Angstroms"], "radiiType"->"VanDerWaalsRadius"};
  (*"AtomicRadius", "CovalentRadius", "VanDerWaalsRadius"*)
  Options[DrawProteinSAS] = {"triangDensity"->5.0, "probeR"->1.5, "toPrint"->False,"rootPath"->$TemporaryDirectory};
  {msmsExePath, pdb2xyzrExePath, pdb2xyzrDatabasePath,reduceExePath, reduceDatabasePath} = InstallMyPckg[];
  reduceDefaultArgs=
  {"-build","-DB","\""<>reduceDatabasePath<>"\""};
  Options[DrawProteinSAS] = {"probeR"->Quantity[1.5,"Angstroms"], "radiiType"->"VanDerWaalsRadius"};
  (*"AtomicRadius", "CovalentRadius", "VanDerWaalsRadius"*)
  Options[ConstructSESmesh] = {"triangDensity"->5.0, "probeR"->1.5, "toPrint"->False,"rootPath"->$TemporaryDirectory};
  
  End[ ];
  
  EndPackage[ ];
