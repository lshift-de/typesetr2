package net.lshift.typesetr.cmd

import java.io.File

import io.circe.Json
import net.lshift.typesetr._

case class Config(inFormat: InputFormat = InputFormat.Odt,
                  outFormat: OutputFormat = OutputFormat.Pdf,
                  inFile: Option[File] = None,
                  outFile: Option[File] = None,
                  style: Style = Style.default(),
                  styleBase: File = new File("/opt/typesetr/styles"),
                  page: Int = 1,
                  pixels: Int = 600,
                  toc: Boolean = true,
                  tocDepth: Int = 3,
                  Yoptimize: Boolean = true,
                  Yns: Boolean = true,
                  Ytmp: Boolean = false,
                  YprettyPrint: Boolean = false,
                  noClean: Boolean = false,
                  gdocMeta: Json = Json.Null,
                  include: List[GDocIdentifier] = Nil,
                  newMeta: Option[YamlString] = None,
                  rewrittenInput: Option[File] = None,
                  lofi: Boolean = false,
                  compress: Option[CompressKind] = None,
                  bibliography: List[File] = Nil,
                  comments: Boolean = true,
                  logging: LogLevel = LogLevel.NoLog)
