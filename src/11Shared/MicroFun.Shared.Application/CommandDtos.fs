namespace MicroFun.Shared.Application


type CommandAsyncResult =
    { entityId: string
      entityVersion: int
      streamSequence: string }
