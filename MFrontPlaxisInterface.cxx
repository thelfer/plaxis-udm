/*!
 * \file   MFrontPlaxisInterface.cxx
 * \brief    
 * \author Thomas Helfer
 * \date   27 août 2024
 */

#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <stdexcept>

namespace mfront::plaxis{

static std::vector<std::string> tokenize(std::string_view s) {
  std::vector<std::string> res;
  auto b = std::string::size_type{};
  auto e = s.find_first_of(" \t", b);
  while (std::string::npos != e || std::string::npos != b) {
    res.push_back(std::string{s.substr(b, e - b)});
    b = s.find_first_not_of(" \t", e);
    e = s.find_first_of(" \t", b);
  }
  return res;
} // end of tokenize

void test() {}

struct MFrontBehaviour{
  std::string library;
  std::string function;
  void (*fct)() = nullptr;
};

struct MFrontBehaviourDataBase
{
  //! \brief return the unique instance of the class
  static const MFrontBehaviourDataBase &get() noexcept(false);
  /*!
   * \brief list of behaviour
   *
   */
  std::vector<MFrontBehaviour> behaviours;
private:
  MFrontBehaviourDataBase() noexcept(false);
  MFrontBehaviourDataBase(MFrontBehaviourDataBase&&) = delete;
  MFrontBehaviourDataBase(const MFrontBehaviourDataBase&) = delete;
  MFrontBehaviourDataBase &operator=(MFrontBehaviourDataBase &&) = delete;
  MFrontBehaviourDataBase &operator=(const MFrontBehaviourDataBase &) = delete;
};

const MFrontBehaviourDataBase &MFrontBehaviourDataBase::get() noexcept(false) {
  static MFrontBehaviourDataBase db;
  return db;
}

MFrontBehaviourDataBase::MFrontBehaviourDataBase() noexcept(false) {
  auto raise = [](const std::string& msg) {
    auto e = std::runtime_error(msg);
    throw(e);
  };
  auto file = std::ifstream{"mfront_plaxis.txt"};
  if (!file.is_open()) {
    raise("failed to open file 'mfront_plaxis.txt'");
  }
  int line_number = 1;
  auto found = false;
  for (std::string line; std::getline(file, line); ++line_number) {
    if (line.starts_with("#")) {
      continue;
    }
    auto tokens = tokenize(line);
    if (tokens.size() != 3) {
      raise("line '" + std::to_string(line_number) +
            "' of file 'mfront_plaxis.txt' is ill-formed");
    }
    const auto id = std::stoi(tokens[0]) - 1;
    if ((id < 0) || (id > 9)) {
      raise("invalid identifier '" + tokens[0] +
            "': the identifier must be great than one and lower than 11");
    }
    if (id != this->behaviours.size()) {
      raise("line '" + std::to_string(line_number) +
            "' of file 'mfront_plaxis.txt' is ill-formed, "
            "the identifer must be '" +
            std::to_string(this->behaviours.size() + 1) + "', not '" +
            tokens[0] + "'");
    }
    this->behaviours.push_back(
        {.library = tokens[2], .function = tokens[1], .fct = test});
  }
  if (this->behaviours.empty()) {
    raise("no behaviour defined in file 'mfront_plaxis.txt'");
  }
  file.close();
} // end of MFrontBehaviourDataBase

} // end of mfront

extern "C"{

int mfront_plaxis_interface(const int task, const int mode) {
  using namespace mfront::plaxis;
  try {
    const auto &db = MFrontBehaviourDataBase::get();
  } catch (std::exception &e) {
    std::cerr << "mfront_plaxis_interface failed: " << e.what() << std::endl;
    return 0;
  } catch (...) {
    std::cerr << "mfront_plaxis_interface failed due to unknown exception"
              << std::endl;
    return 0;
  }
  return 1;
}

} // end of extern "C"