package net.lshift.typesetr.cmd

import java.io.File

import io.circe.Json
import net.lshift.typesetr._

case class Config(format: OutputFormat = OutputFormat.Pdf,
                  inFile: Option[File] = None,
                  outFile: Option[File] = None,
                  style: Style = Style.default(),
                  styleBase: File = new File("/opt/typesetr/styles"),
                  page: Int = 1,
                  pixels: Int = 600,
                  noClean: Boolean = false,
                  gdocMeta: Json = Json.Null,
                  include: List[GDocIdentifier] = Nil,
                  newMeta: Option[YamlString] = None,
                  rewrittenInput: Option[File] = None,
                  lofi: Boolean = false,
                  compress: Option[CompressKind] = None,
                  bibliography: List[File] = Nil,
                  comments: Boolean = true,
                  verbose: Boolean = false,
                  quiet: Boolean = false,
                  error: LogLevel = LogLevel.Log)
