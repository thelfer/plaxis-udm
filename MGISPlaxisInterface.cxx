/*!
 * \file   MGISPlaxisInterface.cxx
 * \brief
 * \author Thomas Helfer
 * \date   27 août 2024
 */

#include <algorithm>
#include <numbers>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>
#include "MGIS/Behaviour/Behaviour.hxx"
#include "MGIS/Behaviour/Integrate.hxx"
#include "MGIS/Behaviour/BehaviourDataView.hxx"


namespace mgis::plaxis {

static void to_buffer(char *const buffer, const std::string_view s) noexcept {
  if (s.size() > 255) {
    std::copy_n(s.begin(), 255, buffer);
  } else {
    std::copy(s.begin(), s.end(), buffer);
    std::fill(buffer + s.size(), buffer + 255, '\0');
  }
} // end of to_buffer

[[noreturn]] static void raise(const std::string &msg) {
  auto e = std::runtime_error(msg);
  throw(e);
} // end of raise
  
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

struct MFrontBehaviourDataBase {
  //! \brief return the unique instance of the class
  static const MFrontBehaviourDataBase &get() noexcept(false);
  /*!
   * \brief list of behaviours
   *
   */
  std::vector<mgis::behaviour::Behaviour> behaviours;

private:
  MFrontBehaviourDataBase() noexcept(false);
  MFrontBehaviourDataBase(MFrontBehaviourDataBase &&) = delete;
  MFrontBehaviourDataBase(const MFrontBehaviourDataBase &) = delete;
  MFrontBehaviourDataBase &operator=(MFrontBehaviourDataBase &&) = delete;
  MFrontBehaviourDataBase &operator=(const MFrontBehaviourDataBase &) = delete;
};

const MFrontBehaviourDataBase &MFrontBehaviourDataBase::get() noexcept(false) {
  static MFrontBehaviourDataBase db;
  return db;
}

MFrontBehaviourDataBase::MFrontBehaviourDataBase() noexcept(false) {
  auto file = std::ifstream{"mfront_plaxis.txt"};
  if (!file.is_open()) {
    raise("failed to open file 'mfront_plaxis.txt'");
  }
  int line_number = 1;
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
    if (id != static_cast<int>(this->behaviours.size())) {
      raise("line '" + std::to_string(line_number) +
            "' of file 'mfront_plaxis.txt' is ill-formed, "
            "the identifer must be '" +
            std::to_string(this->behaviours.size() + 1) + "', not '" +
            tokens[0] + "'");
    }
    this->behaviours.push_back(
        load("lib" + tokens[2] + ".so", tokens[1],
             mgis::behaviour::Hypothesis::TRIDIMENSIONAL));
  }
  if (this->behaviours.empty()) {
    raise("no behaviour defined in file 'mfront_plaxis.txt'");
  }
  file.close();
} // end of MFrontBehaviourDataBase

static const mgis::behaviour::Behaviour& getBehaviour(const int model){
    const auto &db = MFrontBehaviourDataBase::get();
    const auto idx = model - 1;
    if ((idx < 0) || (idx >= static_cast<int>(db.behaviours.size()))) {
      raise("invalid model index '" + std::to_string(model) + "'");
    }
    return db.behaviours.at(idx);
  }

  static void copy_diagonal_component(double *const dest, const double* const src) noexcept {
    dest[0] = src[0];
    dest[1] = src[1];
    dest[2] = src[2];
  }
  
  static void convert_stress_to_mgis(double *const dest, const double* const src) noexcept {
    constexpr auto cste = std::numbers::sqrt2_v<double>;
    copy_diagonal_component(dest, src);
    dest[3] = src[3] * cste;
    dest[4] = src[4] * cste;
    dest[5] = src[5] * cste;
  }

  static void convert_strain_to_mgis(double *const dest, const double* const src) noexcept {
    constexpr auto icste = std::numbers::sqrt2_v<double> / 2;
    copy_diagonal_component(dest, src);
    dest[3] = src[3] * icste;
    dest[4] = src[4] * icste;
    dest[5] = src[5] * icste;
  }

  static void convert_stress_from_mgis(double *const dest, const double* const src) noexcept {
    constexpr auto icste = std::numbers::sqrt2_v<double> / 2;
    copy_diagonal_component(dest, src);
    dest[3] = src[3] * icste;
    dest[4] = src[4] * icste;
    dest[5] = src[5] * icste;
  }

  static void convert_tangent_operator_from_mgis(double *dest, const double* const src) noexcept {
    std::copy(src, src+ 36, dest);
  }

  /*!
   * \param[in] model: behaviour identifier
   * \param[in] D: tangent operator
   * \param[in] sig: stress tensor at the end of the time step
   * \param[in] isvs: internal state variables at the end of the time step
   * \param[in] mps: material properties
   * \param[in] sig0: stress tensor at the beginning of the time step
   * \param[in] isvs0: internal state variables at the beginning of the time step
   * \param[in] deps: strain increment and strain the beginning of the time step
   * \param[in] dt: time increment
   */
  int integrate(mgis::behaviour::IntegrationType it,
		const int model,
		double* const D,
		double* const sig,
		double* const isvs,
		const double* const mps,
		const double* const sig0,
		const double* const isvs0,
		const double* const deps,
		const double T,
		const double dt) {
    const auto &b = getBehaviour(model);
    char emsg[255];
    double K[6];
    double eto_bts[6];
    double eto_ets[6];
    double sig_bts[6];
    double sig_ets[6];
    // convert strains
    convert_strain_to_mgis(eto_bts, deps+6);
    convert_strain_to_mgis(eto_ets, deps);
    for(int i=0; i!=6; ++i){
      eto_ets[i] += eto_bts[i];
    }
    // convert stress at the beginning of the time step
    convert_stress_to_mgis(sig_bts, sig0);
    // creating the BehaviourDataView
    auto rdt = double{1};
    auto bdata = mgis::behaviour::BehaviourDataView{};
    bdata.error_message = emsg;
    bdata.dt = dt;
    bdata.K = D;
    bdata.K[0] = static_cast<int>(it);
    bdata.rdt = &rdt;
    bdata.speed_of_sound = nullptr;
    bdata.s0.gradients = eto_bts;
    bdata.s0.thermodynamic_forces = sig_bts;
    bdata.s0.mass_density = nullptr;
    bdata.s0.material_properties = mps;
    bdata.s0.internal_state_variables = isvs0;
    bdata.s0.stored_energy = nullptr;
    bdata.s0.dissipated_energy = nullptr;
    bdata.s0.external_state_variables = &T;      
    bdata.s1.gradients = eto_ets;
    bdata.s1.thermodynamic_forces = sig_ets;
    bdata.s1.mass_density = nullptr;
    bdata.s1.material_properties = mps;
    bdata.s1.internal_state_variables = isvs;
    bdata.s1.stored_energy = nullptr;
    bdata.s1.dissipated_energy = nullptr;
    bdata.s1.external_state_variables = &T;      
    const auto r = integrate(bdata, b);
    if((r!=0) && (r!=1)){
      return 0;
    }
    // convert results
    convert_stress_from_mgis(sig, sig_ets);
    convert_tangent_operator_from_mgis(D, K);
    return 1;
  } // end of integrate
  
} // namespace mgis::plaxis

extern "C" {

static int handle_cxx_exceptions(const char *const caller) noexcept {
  try {
    throw;
  } catch (std::exception &e) {
    std::cerr << caller << "failed: " << e.what() << std::endl;
  } catch (...) {
    std::cerr << caller << "failed due to unknown exception" << std::endl;
  }
  return 0;
}

int mgis_plaxis_interface_get_model_count() {
  using namespace mgis::plaxis;
  try {
    const auto &db = MFrontBehaviourDataBase::get();
    return static_cast<int>(db.behaviours.size());
  } catch (...) {
    return handle_cxx_exceptions("mgis_plaxis_interface_get_model_count");
  }
  return 1;
}
  
void mgis_plaxis_interface_get_model_name(char *const name, const int model) {
  using namespace mgis::plaxis;
  try {
    const auto &b = getBehaviour(model);
    to_buffer(name, b.function);
  } catch (...) {
    handle_cxx_exceptions("mgis_plaxis_interface_get_model_name");
  }
}

int mgis_plaxis_interface_get_material_properties_count(const int model) {
  using namespace mgis::plaxis;
  try {
    const auto &b = getBehaviour(model);
    return static_cast<int>(b.mps.size());
  } catch (...) {
    handle_cxx_exceptions("mgis_plaxis_interface_get_material_properties_count");
  }
  return 0;
}

void mgis_plaxis_interface_get_material_property_name(char *const name,
						      const int model,
						      const int i) {
  using namespace mgis::plaxis;
  try {
    const auto &b = getBehaviour(model);
    const auto idx = i - 1;
    if ((idx < 0) || (idx >= static_cast<int>(b.mps.size()))) {
      raise("invalid material property index '" + std::to_string(i) +
	    "' for model '"+b.function+"' ("+std::to_string(model)+")");
    }
    const auto& mp = b.mps.at(idx);
    to_buffer(name, mp.name);
  } catch (...) {
    handle_cxx_exceptions("mgis_plaxis_interface_get_material_property_name");
  }
}
  
int mgis_plaxis_interface_get_internal_state_variables_count(const int model) {
  using namespace mgis::plaxis;
  try {
    const auto &b = getBehaviour(model);
    return static_cast<int>(b.isvs.size());
  } catch (...) {
    handle_cxx_exceptions("mgis_plaxis_interface_get_internal_state_variables_count");
  }
  return 0;
}

void mgis_plaxis_interface_get_state_variable_name(char *const name,
						      const int model,
						      const int i) {
  using namespace mgis::plaxis;
  try {
    const auto &b = getBehaviour(model);
    const auto idx = i - 1;
    if ((idx < 0) || (idx >= static_cast<int>(b.isvs.size()))) {
      raise("invalid state variable index '" + std::to_string(i) +
	    "' for model '"+b.function+"' ("+std::to_string(model)+")");
    }
    const auto& mp = b.isvs.at(idx);
    to_buffer(name, mp.name);
  } catch (...) {
    handle_cxx_exceptions("mgis_plaxis_interface_get_state_variable_name");
  }
}

  /*!
   * \param[in] task: task to be performed
   * \param[in] model: behaviour identifier
   * \param[out] nisvs: number of state variables 
   * \param[out] unsymmetric: integer stating if the tangent operator is unsymmetric
   * \param[out] stressDependent: integer stating if the tangent operator is stress dependent
   * \param[out] timeDependent: integer stating if the tangent operator is time dependent
   * \param[out] constistentTangentOperator:  integer stating if the tangent operator is consistent
   * \param[out] D: tangent operator
   * \param[out] sig: stress tensor at the end of the time step
   * \param[out] isvs: internal state variables at the end of the time step
   * \param[in] mps: material properties
   * \param[in] sig0: stress tensor at the beginning of the time step
   * \param[in] isvs0: internal state variables at the beginning of the time step
   * \param[in] deps: strain increment and strain the beginning of the time step
   * \param[in] dt: time increment
   */
  int mgis_plaxis_interface(const int task, const int model,
			    int *const nisvs,
			    int *const unsymmetric,
			    int *const stressDependent,
			    int *const timeDependent,
			    int *const constistentTangentOperator,
			    double* const D,
			    double* const sig,
			    double* const isvs,
			    const double* const mps,
			    const double* const sig0,
			    const double* const isvs0,
			    const double* const deps,
			    const double dt) {
    using namespace mgis::behaviour;
    using namespace mgis::plaxis;
  try {
    if(task == 1){
      // initialize state variables
      const auto n = mgis_plaxis_interface_get_internal_state_variables_count(model);
      for(int i=0; i!=n; ++i){
	isvs[i] = double{};
      }
    } else if(task == 2){
      constexpr auto it = IntegrationType::INTEGRATION_NO_TANGENT_OPERATOR;
      integrate(it, model, D, sig, isvs, mps,sig0, isvs0, deps, 293.15, dt);
    } else  if(task == 3){
      constexpr auto it = IntegrationType::INTEGRATION_CONSISTENT_TANGENT_OPERATOR;
      integrate(it, model, D, sig, isvs, mps,sig0, isvs0, deps, 293.15, dt);
    } else  if(task == 4){
      // number of state variables
      *nisvs = mgis_plaxis_interface_get_internal_state_variables_count(model);
    } else  if(task == 5){
      // matrix attributes
      *unsymmetric = 1;
      *stressDependent = 1;
      *timeDependent = 1;
      *constistentTangentOperator = 1;
    } else  if(task == 6){
      // elastic stiffness
      const auto it = IntegrationType::PREDICTION_ELASTIC_OPERATOR;
      integrate(it, model, D, sig, isvs, mps,sig0, isvs0, deps, 293.15, dt);
    } else {
      raise("invalid task '" + std::to_string(task) + "' ");
    }
  } catch (...) {
    return handle_cxx_exceptions("mgis_plaxis_interface");
  }
  return 1;
}
  
} // end of extern "C"
