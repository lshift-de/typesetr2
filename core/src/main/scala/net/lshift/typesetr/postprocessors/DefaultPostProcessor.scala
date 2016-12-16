package net.lshift.typesetr
package postprocessors

import net.lshift.typesetr.pandoc.UUIDGen
import parsers.NodeConfigs

object DefaultPostProcessor {

  private class OptimizerAndMetaExtractor[T](config: NodeConfigs.WithNode[T], uuidGen: UUIDGen)
    extends PostProcessor[T]
    with Optimizer[T]
    with PostProcessorUtils[T]
    with OptimizerCoalesceParentChild[T]
    with OptimizerCoalesceBlocks[T]
    with OptimizerCoalesceSiblings[T]
    with OptimzerCoalesceHeadings[T]
    with OptimizerCoalesceParagraphs[T]
    with MetaInferencer[T] {

    protected def nodeConfig: NodeConfigs.WithNode[T] = config
    protected def uuid: UUIDGen = uuidGen

  }

  def fromConfig[T](config: NodeConfigs.WithNode[T])(implicit uuid: UUIDGen): PostProcessor[T] =
    new OptimizerAndMetaExtractor(config, uuid)

}
