import torch
import torch.nn as nn

class MyBlock(nn.Module):
    """
    The __init__ is where layers are declared, runs once when the module is created 
    """
    def __init__(self, in_channels, out_channels):
        super().__init__() # Calling the parent class first (required)
        # Defining learnable layers here – get registered automatically
        self.conv = nn.Conv2d(in_channels, out_channels, kernel_size=3, padding=1)
        self.bn = nn.BatchNorm2d(out_channels)
        self.relu = nn.ReLU(inplace=True)
    """
    forward is where layers are used, runs every time you pass data through it
    """
    def forward(self, x):
        # What happens when you call the module on input x
        x = self.conv(x)
        x = self.bn(x)
        x = self.relu(x)
        return(x)
    """
    Calling block(x) is the same as block.foward(x) since PyTorch sets this up via
    __call__ that is passed via super().__init__()
    """

#Usage

block = MyBlock(in_channels=32, out_channels=64)
fake_input = torch.randn(2, 32, 64, 64) #(batch, channels, H, W)
output = block(fake_input) # calls forward() as well
print(output.shape) # should be torch.Size([2, 64, 64, 64])

class UnifyingConv(nn.Module):
    def __init__(self, in_channels, out_channels=64):
        super().__init__()
        self.conv = nn.Conv2d(in_channels, out_channels, kernel_size=3, padding=1)
        self.bn = nn.BatchNorm2d(out_channels)
        self.relu = nn.ReLU(inplace=True)
    
    def forward(self, x):
        x = self.conv(x)
        x = self.bn(x)
        x = self.relu(x)
        return(x)

fake_tensor = torch.randn(1, 32, 64, 64)
testUnif = UnifyingConv(in_channels=32)
test_output = testUnif(fake_tensor)
print(test_output.shape)

########################################

class InputBranch(nn.Module):
    def __init__(self, in_channels, out_channels, scale_factor):
        super().__init__()
        self.conv = nn.Conv2d(in_channels, out_channels, kernel_size=3, padding=1)
        self.bn = nn.BatchNorm2d(out_channels)
        self.relu = nn.ReLU(inplace=True)
        if scale_factor > 0:
            self.resample = nn.MaxPool2d(kernel_size=scale_factor)
        elif scale_factor < 0:
            self.resample = nn.Upsample(scale_factor=abs(scale_factor), mode = "bilinear", align_corners=False)
        else:
            self.resample = nn.Identity()
        
    def forward(self, x):
        x = self.resample(x)
        x = self.conv(x)
        x = self.bn(x)
        x = self.relu(x)
        return(x)

testInputBranch = InputBranch(in_channels=32, out_channels=64, scale_factor=-4)
test_IB_output = testInputBranch(fake_tensor)
print(test_IB_output.shape)

b1 = InputBranch(in_channels=32,  out_channels=64, scale_factor=4)    # E1 → D3
b2 = InputBranch(in_channels=128, out_channels=64, scale_factor=0)    # E3 → D3
b3 = InputBranch(in_channels=512, out_channels=64, scale_factor=-4)   # BN → D3

print(b1(torch.randn(1, 32,  256, 256)).shape)   # expect (1, 64, 64, 64)
print(b2(torch.randn(1, 128, 64,  64)).shape)    # expect (1, 64, 64, 64)
print(b3(torch.randn(1, 512, 16,  16)).shape)    # expect (1, 64, 64, 64)