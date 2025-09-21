# Homebrew formula for Traverse
# To install: brew install --formula https://raw.githubusercontent.com/calltrace/traverse/main/HomebrewFormula/traverse.rb
class Traverse < Formula
  desc "Solidity code analysis, visualization, and test generation tools"
  homepage "https://github.com/calltrace/traverse"
  version "0.1.2"
  license "MIT"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/calltrace/traverse/releases/download/v#{version}/traverse-macos-arm64.tar.gz"
      sha256 "784282cc2ccfe07dc50671bbc1ea8ef57d83de02e49fcd23bd8e423f041cda7d"
    else
      url "https://github.com/calltrace/traverse/releases/download/v#{version}/traverse-macos-amd64.tar.gz"
      sha256 "6bde3109db57d70ec0ecf84133853a5ce22a5b3b88f8d4fc4e4f6315d06619ec"
    end
  end

  on_linux do
    url "https://github.com/calltrace/traverse/releases/download/v#{version}/traverse-linux-amd64.tar.gz"
    sha256 "30cbe29a3bc418ea1d12a3ae7e6b6edbd8124d257b6cd05d3489ec7e2896c3cf"
  end

  def install
    bin.install "sol2cg"
    bin.install "sol2test"
    bin.install "sol2bnd"
    bin.install "sol-storage-analyzer"
    bin.install "storage-trace"
  end

  test do
    # Basic version checks
    assert_match "sol2cg", shell_output("#{bin}/sol2cg --version")
    
    # Functional test
    (testpath/"test.sol").write <<~EOS
      contract Test {
        function test() public {}
      }
    EOS
    
    system bin/"sol2cg", testpath/"test.sol", "-o", testpath/"output.dot"
    assert_predicate testpath/"output.dot", :exist?
  end
end