object Service1: TService1
  DisplayName = 'PDF Printer Monitoring Service'
  AfterInstall = ServiceAfterInstall
  OnExecute = ServiceExecute
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 720
  Width = 960
  PixelsPerInch = 144
end
